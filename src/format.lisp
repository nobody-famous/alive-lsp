(defpackage :alive/format
    (:use :cl)
    (:export :range)
    (:local-nicknames (:edit :alive/text-edit)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)))

(in-package :alive/format)


(declaim (optimize (speed 3) (safety 0)))


(defparameter *start-form* 100)

(defparameter *always* 0)
(defparameter *never* 1)
(defparameter *multiline* 2)


(defstruct options
    (indent-width 2)
    (max-blank-lines 2)
    (parens-own-line *never*))


(defclass start-form ()
    ((start :accessor start
            :initform nil
            :initarg :start)
     (end :accessor end
          :initform nil
          :initarg :end)
     (aligned :accessor aligned
              :initform nil
              :initarg :aligned)
     (lambda-list :accessor lambda-list
                  :initform nil
                  :initarg :lambda-list)
     (is-multiline :accessor is-multiline
                   :initform nil
                   :initarg :is-multiline)))


(defmethod print-object ((obj start-form) out)
    (declare (type stream out))
    (format out "{[~A:~A] ML: ~A; aligned: ~A; lambda-list: ~A}"
            (start obj)
            (end obj)
            (is-multiline obj)
            (aligned obj)
            (lambda-list obj)))


(defun new-start-form (token)
    (make-instance 'start-form
                   :start (token:get-start token)
                   :end (token:get-end token)
                   :is-multiline nil))


(defmethod token:get-type-value ((obj start-form))
    *start-form*)


(defmethod token:get-start ((obj start-form))
    (start obj))


(defmethod token:get-end ((obj start-form))
    (end obj))


(defmethod token:get-text ((obj start-form))
    "(")


(defun same-line (token1 token2)
    (and token1
         token2
         (= (the fixnum (pos:line (token:get-start token1)))
            (the fixnum (pos:line (token:get-start token2))))))


(defmethod token:clone ((obj start-form) new-start new-end &optional new-text)
    (declare (ignore new-text))

    (make-instance 'start-form
                   :start new-start
                   :end new-end
                   :is-multiline (is-multiline obj)))


(defstruct parse-state
    tokens
    range
    (indent (list 0))
    edits
    out-list
    seen
    opens
    (options (make-options)))


(defun next-token (state)
    (car (parse-state-tokens state)))


(defun pop-token (state)
    (pop (parse-state-tokens state)))


(defun make-new-token (token start &optional new-str)
    (loop :with line :of-type fixnum := (pos:line start)
          :with col :of-type fixnum := (pos:col start)
          :with str := (if new-str
                           new-str
                           (token:get-text token))

          :for ch :across (the simple-string str) :do
              (cond ((char= #\newline ch)
                     (incf line)
                     (setf col 0))

                    (T (incf col)))

          :finally (return (token:clone token
                                        start
                                        (pos:create line col)
                                        str))))


(defun add-to-out-list (state token)
    (let* ((start (if (car (parse-state-out-list state))
                      (token:get-end (car (parse-state-out-list state)))
                      (pos:create 0 0)))
           (adjusted (make-new-token token start)))

        (push adjusted (parse-state-out-list state))))


(defun pos-out-of-range (range pos)
    (or (pos:less-or-equal pos (range:start range))
        (pos:less-than (range:end range) pos)))


(defun out-of-range (range token)
    (or (pos:less-or-equal (token:get-end token) (range:start range))
        (pos:less-than (range:end range) (token:get-start token))
        (and (token:is-type types:*ws* token)
             (pos:less-or-equal (range:start range) (token:get-start token))
             (pos:less-or-equal (token:get-start token) (range:end range))
             (pos:less-than (range:end range) (token:get-end token)))))


(defun new-line-count (token)
    (let ((start (token:get-start token))
          (end (token:get-end token)))
        (the fixnum (- (the fixnum (pos:line end))
                       (the fixnum (pos:line start))))))


(defun indent-string (nl-count space-count)
    (format nil "~A~A"
            (format nil "~v@{~A~:*~}" nl-count (format nil "~%"))
            (format nil "~v@{~A~:*~}" space-count " ")))


(defun replace-token (state token text)
    (let* ((range (range:create (token:get-start token)
                                (token:get-end token)))
           (edit (edit:create :range range
                              :text text)))

        (push edit (parse-state-edits state))))


(defun insert-text (state pos text)
    (let* ((range (range:create pos pos))
           (edit (edit:create :range range
                              :text text)))

        (unless (pos-out-of-range (parse-state-range state) pos)
                (push edit (parse-state-edits state)))))


(defun replace-indent (state value)
    (pop (parse-state-indent state))
    (push value (parse-state-indent state)))


(defun prev-is-start-form (state)
    (loop :with tokens := (parse-state-seen state)

          :while (and tokens
                      (or (token:is-type types:*ws* (car tokens))
                          (token:is-type types:*line-comment* (car tokens))
                          (token:is-type types:*block-comment* (car tokens))))

          :do (pop tokens)

          :finally (return (and tokens
                                (token:is-type *start-form* (car tokens))))))


(defun has-body (lambda-list)
    (reduce (lambda (acc item)
                (or acc
                    (and (symbolp item)
                         (or (string= item "&BODY")
                             (string= item "&REST")))))
            lambda-list
            :initial-value NIL))


(defun update-aligned (state)
    (let* ((form-open (car (parse-state-opens state)))
           (token (car (parse-state-out-list state))))

        (if (prev-is-start-form state)
            (let ((lambda-list (symbols:get-lambda-list (token:get-text token))))
                (if (has-body lambda-list)
                    (progn (setf (aligned form-open) T)
                           (setf (lambda-list form-open) lambda-list)
                           (replace-indent state (the fixnum (+ (the fixnum (options-indent-width (parse-state-options state)))
                                                                (the fixnum (pos:col (token:get-start token)))
                                                                (the fixnum -1))))
                           (push (the fixnum (+ (the fixnum (* 2
                                                               (the fixnum (options-indent-width (parse-state-options state)))))
                                                (the fixnum (pos:col (token:get-start token)))
                                                (the fixnum -1)))
                                 (parse-state-indent state)))
                    (replace-indent state (pos:col (token:get-start token)))))

            (when (and form-open (not (aligned form-open)))
                  (cond ((and (token:is-type types:*symbol* (car (parse-state-seen state)))
                              (token:is-type types:*colons* token)
                              (token:is-type types:*symbol* (cadr (parse-state-tokens state))))
                         (add-to-out-list state (cadr (parse-state-tokens state)))
                         (pop-token state))

                        ((and (token:is-type types:*colons* (car (parse-state-seen state)))
                              (token:is-type types:*symbol* token))
                         NIL)

                        (T (setf (aligned form-open) T)
                           (replace-indent state (pos:col (token:get-start token)))))))))


(defun fix-indent (state)
    (let* ((indent (car (parse-state-indent state)))
           (token (car (parse-state-seen state)))
           (prev (cadr (parse-state-seen state)))
           (start (token:get-start token))
           (end (token:get-end token)))

        (when (token:is-type types:*ws* token)
              (if (out-of-range (parse-state-range state) token)
                  (add-to-out-list state token)
                  (cond ((or (not prev)
                             (token:is-type *start-form* prev))
                         (replace-token state token ""))

                        ((= (the fixnum (pos:line start)) (the fixnum (pos:line end)))
                         (if (string= " " (the simple-string (token:get-text token)))
                             (add-to-out-list state token)
                             (progn (add-to-out-list state
                                                     (token:create :type-value types:*ws*
                                                                   :start (token:get-start token)
                                                                   :end (pos:create (pos:line start)
                                                                                    (+ (the fixnum 1) (the fixnum (pos:col start))))
                                                                   :text " "))
                                    (replace-token state token " "))))

                        (T
                         (let* ((str (indent-string (new-line-count token) indent))
                                (new-token (make-new-token token (token:get-start token) str)))

                             (add-to-out-list state new-token)
                             (replace-token state token str))))))))


(defun need-space-p (token)
    (not (or (token:is-type types:*quote* token)
             (token:is-type types:*back-quote* token)
             (token:is-type types:*open-paren* token)
             (token:is-type types:*colons* token)
             (token:is-type types:*comma* token)
             (token:is-type types:*comma-at* token)
             (token:is-type types:*macro* token)
             (token:is-type *start-form* token))))


(defun process-open (state token)
    (let* ((prev (car (parse-state-seen state))))

        (when prev
              (cond ((token:is-type types:*ws* prev)
                     (fix-indent state))

                    ((need-space-p prev)
                     (insert-text state (token:get-end prev) " "))))

        (add-to-out-list state token)
        (update-aligned state)

        (push (car (parse-state-out-list state)) (parse-state-opens state))

        (push (pos:col (token:get-end (car (parse-state-out-list state))))
              (parse-state-indent state))))


(defun process-close (state token)
    (let ((prev (car (parse-state-seen state)))
          (prev-prev (cadr (parse-state-seen state))))
        (when (and prev
                   (not (out-of-range (parse-state-range state) prev))
                   (not (eq types:*line-comment* (token:get-type-value prev-prev)))
                   (not (eq types:*block-comment* (token:get-type-value prev-prev)))
                   (eq types:*ws* (token:get-type-value prev)))
              (replace-token state prev "")
              (pop (parse-state-out-list state)))

        (add-to-out-list state token)
        (pop (parse-state-opens state))
        (pop (parse-state-indent state))))


(defun start-of-last-placed (state)
    (let* ((item (car (parse-state-out-list state))))
        (if item
            (pos:col (token:get-start item))
            0)))


(defun process-token (state token)
    (let ((prev (car (parse-state-seen state))))

        (when prev
              (cond ((or (token:is-type types:*line-comment* token)
                         (token:is-type types:*block-comment* token))
                     (if (and (token:is-type types:*ws* prev)
                              (not (out-of-range (parse-state-range state) prev))
                              (same-line prev token))
                         (when (not (string= " " (the simple-string (token:get-text prev))))
                               (replace-token state prev " "))
                         (fix-indent state)))

                    ((token:is-type types:*ws* prev) (fix-indent state))

                    ((and (not (token:is-type types:*colons* token))
                          (need-space-p prev))
                     (insert-text state (token:get-end prev) " "))))

        (add-to-out-list state token)

        (update-aligned state)))


(defun check-end-space (state)
    (let ((token (car (parse-state-seen state))))
        (when (and token
                   (not (out-of-range (parse-state-range state) token))
                   (token:is-type types:*ws* token))
              (replace-token state token ""))))


(defun convert-tokens (tokens)
    (loop :with converted := '()
          :with opens = '()

          :for token :in tokens :do
              (cond ((token:is-type types:*open-paren* token)
                     (let ((new-token (new-start-form token)))
                         (push new-token opens)
                         (push new-token converted)))

                    ((= (the fixnum types:*close-paren*) (the fixnum (token:get-type-value token)))
                     (pop opens)
                     (push token converted))

                    ((token:is-type types:*ws* token) (push token converted))

                    (T (when (and (car opens)
                                  (not (= (the fixnum (pos:line (token:get-start (car opens))))
                                          (the fixnum (pos:line (token:get-end token))))))
                             (setf (is-multiline (car opens)) T))
                       (push token converted)))

          :finally (return (reverse converted))))


(defun range (input range)
    (let* ((tokens (convert-tokens (tokenizer:from-stream input)))
           (state (make-parse-state :tokens tokens
                                    :range range)))

        (loop :while (parse-state-tokens state)

              :do (let ((token (next-token state))
                        (form-open (car (parse-state-opens state))))

                      (when (and form-open
                                 (lambda-list form-open)
                                 (not (token:is-type types:*ws* token)))
                            (when (and (not (eq 'cons (type-of (car (lambda-list form-open)))))
                                       (or (string= (the symbol (car (lambda-list form-open))) "&BODY")
                                           (string= (the symbol (car (lambda-list form-open))) "&REST")))
                                  (pop (parse-state-indent state)))
                            (pop (lambda-list form-open)))

                      (cond ((token:is-type *start-form* token) (process-open state token))
                            ((token:is-type types:*close-paren* token) (process-close state token))
                            ((token:is-type types:*ws* token) nil)
                            (T (process-token state token)))

                      (push token (parse-state-seen state))
                      (pop-token state))

              :finally (progn (check-end-space state)
                              (return (reverse (parse-state-edits state)))))))
