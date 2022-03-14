(defpackage :alive/format
    (:use :cl)
    (:export :range)
    (:local-nicknames (:edit :alive/text-edit)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)))

(in-package :alive/format)


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
     (is-multiline :accessor is-multiline
                   :initform nil
                   :initarg :is-multiline)))


(defmethod print-object ((obj start-form) out)
    (format out "{[~A:~A]~A}"
            (start obj)
            (end obj)
            (is-multiline obj)))


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
    (options (make-options)))


(defun next-token (state)
    (car (parse-state-tokens state)))


(defun pop-token (state)
    (pop (parse-state-tokens state)))


(defun make-new-token (token start &optional new-str)
    (loop :with line := (pos:line start)
          :with col := (pos:col start)
          :with str := (if new-str
                           new-str
                           (token:get-text token))

          :for ch :across str :do
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


(defun out-of-range (range token)
    (or (pos:less-or-equal (token:get-end token) (range:start range))
        (pos:less-than (range:end range) (token:get-start token))))


(defun new-line-count (token)
    (let ((start (token:get-start token))
          (end (token:get-end token)))
        (- (pos:line end) (pos:line start))))


(defun indent-string (nl-count space-count)
    (format nil "~A~A"
            (format nil "~v@{~A~:*~}" nl-count (format nil "~%"))
            (format nil "~v@{~A~:*~}" space-count " ")))


(defun fix-indent (state)
    (let* ((indent (car (parse-state-indent state)))
           (token (car (parse-state-seen state)))
           (prev (cadr (parse-state-seen state)))
           (start (token:get-start token))
           (end (token:get-end token)))

        (when (and (not (out-of-range (parse-state-range state) token))
                   (= types:*ws* (token:get-type-value token)))

              (cond ((or (not prev)
                         (= *start-form* (token:get-type-value prev)))
                     (replace-token state token ""))

                    ((= (pos:line start) (pos:line end))
                     (if (string= " " (token:get-text token))
                         (add-to-out-list state token)
                         (progn (add-to-out-list state
                                                 (token:create :type-value types:*ws*
                                                               :start (token:get-start token)
                                                               :end (pos:create (pos:line start) (+ 1 (pos:col start)))
                                                               :text " "))
                                (replace-token state token " "))))

                    (T
                     (let* ((str (indent-string (new-line-count token) indent))
                            (new-token (make-new-token token (token:get-start token) str)))

                         (add-to-out-list state new-token)
                         (replace-token state token str)))))))


(defun process-open (state token)
    (let* ((prev (car (parse-state-seen state))))

        (when (and prev
                   (= types:*ws* (token:get-type-value prev)))
              (fix-indent state))

        (add-to-out-list state token)

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
        (pop (parse-state-indent state))))


(defun replace-token (state token text)
    (let* ((range (range:create (token:get-start token)
                                (token:get-end token)))
           (edit (edit:create :range range
                              :text text)))
        (push edit (parse-state-edits state))))


(defun last-token-p (state)
    (= 1 (length (parse-state-tokens state))))


(defun process-token (state token)
    (let ((prev (car (parse-state-seen state))))

        (cond ((or (= types:*line-comment* (token:get-type-value token))
                   (= types:*block-comment* (token:get-type-value token)))
               (when (and prev
                          (= types:*ws* (token:get-type-value prev))
                          (= (pos:line (token:get-start prev))
                             (pos:line (token:get-start token)))
                          (not (string= " " (token:get-text prev))))
                     (replace-token state prev " ")))

              ((and prev
                    (= types:*ws* (token:get-type-value prev)))
               (fix-indent state)))

        (add-to-out-list state token)))


(defun check-end-space (state)
    (let ((token (car (parse-state-seen state))))
        (when (and token
                   (not (out-of-range (parse-state-range state) token))
                   (= types:*ws* (token:get-type-value token)))
              (replace-token state token ""))))


(defun convert-tokens (tokens)
    (loop :with converted := '()
          :with opens = '()

          :for token :in tokens :do
              (cond ((= types:*open-paren* (token:get-type-value token))
                     (let ((new-token (new-start-form token)))
                         (push new-token opens)
                         (push new-token converted)))

                    ((= types:*close-paren* (token:get-type-value token))
                     (pop opens)
                     (push token converted))

                    ((= types:*ws* (token:get-type-value token)) (push token converted))

                    (T (when (and (car opens)
                                  (not (= (pos:line (token:get-start (car opens)))
                                          (pos:line (token:get-end token)))))
                             (setf (is-multiline (car opens)) T))
                       (push token converted)))

          :finally (return (reverse converted))))


(defun range (input range)
    (let* ((tokens (convert-tokens (tokenizer:from-stream input)))
           (state (make-parse-state :tokens tokens
                                    :range range)))

        (loop :while (parse-state-tokens state)

              :do (let ((token (next-token state)))
                      (unless (out-of-range range token)
                              (cond ((= *start-form* (token:get-type-value token)) (process-open state token))
                                    ((= types:*close-paren* (token:get-type-value token)) (process-close state token))
                                    ((= types:*ws* (token:get-type-value token)) nil)
                                    (T (process-token state token))))

                      (push token (parse-state-seen state))
                      (pop-token state))

              :finally (progn (check-end-space state)
                              (return (reverse (parse-state-edits state)))))))
