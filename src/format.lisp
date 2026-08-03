(defpackage :alive/format
    (:use :cl)
    (:export :on-type
             :eol
             :range)
    (:local-nicknames (:edit :alive/text-edit)
                      (:form :alive/parse/form)
                      (:packages :alive/packages)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)))

(in-package :alive/format)


(declaim (optimize (speed 3)))


(defparameter *force-align-targets* (list "and" "or" "cond" "not"
                                          "when" "if" "progn" "loop"
                                          "/" "*" "-" "+" "<" "<=" ">" ">="
                                          "eq" "equal" "equalp" "list"))

(defparameter *loop-keys* '("do" "for" "while" "until" "=" "from" "to" "with" "finally" "repeat" "collect" "into" "using"
                                 "being" "the" "hash-keys" "of"))

(defparameter *always* 0)
(defparameter *never* 1)
(defparameter *multiline* 2)


(defstruct options
    (indent-width 2)
    (max-blank-lines 2)
    (parens-own-line *never*))


(defstruct parse-state
    xyz-tokens
    range
    (indent (list 0))
    edits
    xyz-out-list
    xyz-seen
    xyz-opens
    cur-pkg
    (options (make-options)))


(defun xyz-same-line (token1 token2)
    (and token1
         token2
         (= (the fixnum (pos:line (token:xyz-get-start token1)))
             (the fixnum (pos:line (token:xyz-get-start token2))))))


(defun xyz-next-token (state)
    (car (parse-state-xyz-tokens state)))


(defun xyz-next-next-token (state)
    (cadr (parse-state-xyz-tokens state)))


(defun xyz-pop-token (state)
    (pop (parse-state-xyz-tokens state)))


(defun xyz-make-new-token (token start &optional new-str)
    (loop :with line :of-type fixnum := (pos:line start)
          :with col :of-type fixnum := (pos:col start)
          :with str := (or new-str (token:xyz-get-text token))

          :for ch :across (the simple-string str) :do
              (cond ((char= #\newline ch)
                        (incf line)
                        (setf col 0))

                    (T (incf col)))

          :finally (return (token:xyz-create :type-value (token:xyz-get-type-value token)
                                             :start start
                                             :end (pos:create line col)
                                             :text str))))


(defun xyz-add-to-out-list (state token)
    (let* ((start (if (car (parse-state-xyz-out-list state))
                      (token:xyz-get-end (car (parse-state-xyz-out-list state)))
                      (pos:create 0 0)))
           (adjusted (xyz-make-new-token token start)))

        (push adjusted (parse-state-xyz-out-list state))))


(defun pos-out-of-range (range pos)
    (or (pos:less-or-equal pos (range:start range))
        (pos:less-than (range:end range) pos)))


(defun xyz-out-of-range (range token)
    (or (pos:less-or-equal (token:xyz-get-end token) (range:start range))
        (pos:less-than (range:end range) (token:xyz-get-start token))
        (and (token:xyz-is-type types:*ws* token)
             (pos:less-or-equal (range:start range) (token:xyz-get-start token))
             (pos:less-or-equal (token:xyz-get-start token) (range:end range))
             (pos:less-than (range:end range) (token:xyz-get-end token)))))


(defun xyz-new-line-count (token)
    (let ((start (token:xyz-get-start token))
          (end (token:xyz-get-end token)))
        (the fixnum (- (the fixnum (pos:line end))
                       (the fixnum (pos:line start))))))


(defun do-indent (out num str)
    (declare (type stream out)
             (type fixnum num))

    (loop :repeat num
          :do (format out "~A" str)))


#+win32 (defparameter EOL (format nil "~A~A" #\return #\newline))
#-win32 (defparameter EOL (format nil "~A" #\newline))


(defun indent-string (nl-count space-count)
    (let ((out (make-string-output-stream)))
        (do-indent out nl-count (format nil "~A" EOL))
        (do-indent out space-count " ")
        (get-output-stream-string out)))


(defun xyz-replace-token (state token text)
    (declare (optimize (speed 0)))

    (let* ((range (range:create (token:xyz-get-start token)
                                (token:xyz-get-end token)))
           (edit (edit:create :range range
                              :text text)))

        (unless (string= text (token:xyz-get-text token))
            (push edit (parse-state-edits state)))))


(defun insert-text (state pos text)
    (let* ((range (range:create pos pos))
           (edit (edit:create :range range
                              :text text)))

        (unless (pos-out-of-range (parse-state-range state) pos)
            (push edit (parse-state-edits state)))))


(defun replace-indent (state value)
    (pop (parse-state-indent state))
    (push value (parse-state-indent state)))


(defun get-next-indent (state)
    (let ((indent (car (parse-state-indent state))))
        (if (eq 'cons (type-of indent))
            (car indent)
            (or indent 0))))


(defun pop-next-indent (state)
    (let ((indent (car (parse-state-indent state))))
        (if (eq 'cons (type-of indent))
            (replace-indent state (cdr indent))
            (pop (parse-state-indent state)))))


(defun xyz-prev-is-start-form (state)
    (loop :with tokens := (parse-state-xyz-seen state)

          :while (and tokens
                      (or (token:xyz-is-type types:*ws* (car tokens))
                          (token:xyz-is-type types:*line-comment* (car tokens))
                          (token:xyz-is-type types:*block-comment* (car tokens))))

          :do (pop tokens)

          :finally (return (and tokens
                                (token:xyz-is-type types:*open-paren* (car tokens))))))


(defun has-body (lambda-list)
    (reduce (lambda (acc item)
                (or acc
                    (and (symbolp item)
                         (or (string= item "&BODY")
                             (string= item "&REST")))))
            lambda-list
        :initial-value NIL))


(defun xyz-set-cur-pkg (state)
    (let ((token1 (car (parse-state-xyz-tokens state)))
          (token2 (cadr (parse-state-xyz-tokens state))))
        (cond ((and (token:xyz-is-type types:*colons* token1)
                    (token:xyz-is-type types:*symbol* token2))
                  (setf (parse-state-cur-pkg state)
                      (format nil "~A~A"
                          (token:xyz-get-text token1)
                          (token:xyz-get-text token2))))
              ((token:xyz-is-type types:*macro* token1)
                  (setf (parse-state-cur-pkg state)
                      (token:xyz-get-text token1))))))


(defun xyz-lookup-lambda-list (token1 token2 token3)
    (cond ((and (token:xyz-is-type types:*symbol* token1)
                (token:xyz-is-type types:*colons* token2)
                (token:xyz-is-type types:*symbol* token3))

              (symbols:get-lambda-list (token:xyz-get-text token3)
                                       (token:xyz-get-text token1)))

          ((and (token:xyz-is-type types:*symbol* token1)
                (not (token:xyz-is-type types:*colons* token2)))

              (symbols:get-lambda-list (token:xyz-get-text token1)
                                       (package-name *package*)))))


(defun xyz-force-aligned-p (token)
    (let ((name (string-downcase (token:xyz-get-text token))))
        (member name *force-align-targets* :test #'string=)))


(defun xyz-token-is (token text)
    (declare (type simple-string text))
    (string-equal text (token:xyz-get-text token)))


(defun xyz-align-first-item (state token form-open prev-open)
    (let* ((ns (car (parse-state-xyz-tokens state)))
           (colons (cadr (parse-state-xyz-tokens state)))
           (sym (caddr (parse-state-xyz-tokens state)))
           (lambda-list (xyz-lookup-lambda-list ns colons sym)))

        (when (string-equal "in-package" (token:xyz-get-text token))
              (setf (parse-state-cur-pkg state) NIL))

        (cond ((xyz-token-is token "cond") (token:set-is-cond form-open T)
                                           (replace-indent state (pos:col (token:xyz-get-start token))))

              ((xyz-token-is token "loop") (token:set-is-loop form-open T)
                                           (replace-indent state (pos:col (token:xyz-get-start token))))

              ((and prev-open (token:cond-p prev-open))
                  (replace-indent state (the fixnum (+ (the fixnum (options-indent-width (parse-state-options state)))
                                                       (the fixnum (pos:col (token:xyz-get-start token)))
                                                       (the fixnum -1)))))

              ((xyz-force-aligned-p token) (replace-indent state (pos:col (token:xyz-get-start token))))

              ((has-body lambda-list) (token:set-aligned form-open T)
                                      (token:set-lambda-list form-open lambda-list)
                                      (replace-indent state (cons (the fixnum (+ (the fixnum (* 2
                                                                                                (the fixnum (options-indent-width (parse-state-options state)))))
                                                                                 (the fixnum (pos:col (token:xyz-get-start token)))
                                                                                 (the fixnum -1)))
                                                                  (the fixnum (+ (the fixnum (options-indent-width (parse-state-options state)))
                                                                                 (the fixnum (pos:col (token:xyz-get-start token)))
                                                                                 (the fixnum -1))))))

              (lambda-list (replace-indent state (the fixnum (+ (the fixnum (options-indent-width (parse-state-options state)))
                                                                (the fixnum (pos:col (token:xyz-get-start token)))
                                                                (the fixnum -1)))))

              (T (replace-indent state (pos:col (token:xyz-get-start token)))))))


(defun xyz-align-next-item (state token form-open)
    (cond ((and (token:xyz-is-type types:*symbol* (car (parse-state-xyz-seen state)))
                (token:xyz-is-type types:*colons* token)
                (token:xyz-is-type types:*symbol* (cadr (parse-state-xyz-tokens state))))
              (xyz-add-to-out-list state (cadr (parse-state-xyz-tokens state)))
              (xyz-pop-token state))

          ((and (token:xyz-is-type types:*colons* (car (parse-state-xyz-seen state)))
                (token:xyz-is-type types:*symbol* token))
              NIL)

          (T (unless (parse-state-cur-pkg state)
                 (xyz-set-cur-pkg state))
             (token:set-aligned form-open T)
             (replace-indent state (pos:col (token:xyz-get-start token))))))


(defun xyz-update-aligned (state)
    (let* ((cur-open (car (parse-state-xyz-opens state)))
           (prev-open (cadr (parse-state-xyz-opens state)))
           (token (car (parse-state-xyz-out-list state)))
           (prev (cadr (parse-state-xyz-out-list state)))
           (pkg (packages:for-string (parse-state-cur-pkg state)))
           (*package* (or pkg *package*)))

        (cond ((xyz-prev-is-start-form state)
                  (xyz-align-first-item state token cur-open prev-open))

              ((and cur-open
                    (token:loop-p cur-open)
                    (not (token:aligned-p cur-open))
                    (xyz-is-loop-key state token)
                    (token:xyz-is-type types:*ws* prev))
                  (token:set-aligned cur-open T)
                  (token:set-is-loop cur-open T)
                  (replace-indent state (the fixnum (+ (the fixnum (options-indent-width (parse-state-options state)))
                                                       (the fixnum (pos:col (token:xyz-get-start token)))))))

              ((and cur-open
                    (not (token:aligned-p cur-open)))
                  (xyz-align-next-item state token cur-open)))))


(defun xyz-fix-indent (state)
    (let* ((indent (get-next-indent state))
           (token (car (parse-state-xyz-seen state)))
           (prev (cadr (parse-state-xyz-seen state)))
           (next (xyz-next-token state))
           (nl-count (if (zerop (list-length (parse-state-xyz-opens state)))
                         (min (xyz-new-line-count token) 3)
                         (min (xyz-new-line-count token) 2)))
           (start (token:xyz-get-start token))
           (end (token:xyz-get-end token)))

        (when (and next
                   (xyz-is-loop-key state next)
                   (token:xyz-is-multiline token))
              (decf (the fixnum indent)
                    (the fixnum (options-indent-width (parse-state-options state)))))

        (when (token:xyz-is-type types:*ws* token)
              (if (xyz-out-of-range (parse-state-range state) token)
                  (xyz-add-to-out-list state token)
                  (cond ((or (not prev)
                             (token:xyz-is-type types:*open-paren* prev))
                            (xyz-replace-token state token ""))

                        ((= (the fixnum (pos:line start)) (the fixnum (pos:line end)))
                            (if (string-equal " " (token:xyz-get-text token))
                                (xyz-add-to-out-list state token)
                                (progn (xyz-add-to-out-list state
                                                            (token:xyz-create :type-value types:*ws*
                                                                              :start (token:xyz-get-start token)
                                                                              :end (pos:create (pos:line start)
                                                                                               (+ (the fixnum 1) (the fixnum (pos:col start))))
                                                                              :text " "))
                                       (xyz-replace-token state token " "))))

                        (T (let* ((str (indent-string nl-count indent))
                                  (new-token (xyz-make-new-token token (token:xyz-get-start token) str)))
                               (xyz-add-to-out-list state new-token)
                               (xyz-replace-token state token str))))))))


(defun xyz-need-space-p (token)
    (not (or (token:xyz-is-type types:*quote* token)
             (token:xyz-is-type types:*back-quote* token)
             (token:xyz-is-type types:*open-paren* token)
             (token:xyz-is-type types:*colons* token)
             (token:xyz-is-type types:*comma* token)
             (token:xyz-is-type types:*comma-at* token)
             (token:xyz-is-type types:*macro* token))))


(defun xyz-process-open (state token)
    (let* ((prev (car (parse-state-xyz-seen state))))
        (when prev
              (cond ((token:xyz-is-type types:*ws* prev)
                        (xyz-fix-indent state))

                    ((xyz-need-space-p prev)
                        (insert-text state (token:xyz-get-end prev) " "))))

        (xyz-add-to-out-list state token)
        (xyz-update-aligned state)

        (push (car (parse-state-xyz-out-list state)) (parse-state-xyz-opens state))

        (push (pos:col (token:xyz-get-end (car (parse-state-xyz-out-list state))))
              (parse-state-indent state))))


(defun xyz-process-close (state token)
    (let ((prev (car (parse-state-xyz-seen state)))
          (prev-prev (cadr (parse-state-xyz-seen state))))

        (when (and prev
                   (not (xyz-out-of-range (parse-state-range state) prev))
                   (not (eq types:*line-comment* (token:xyz-get-type-value prev-prev)))
                   (not (eq types:*block-comment* (token:xyz-get-type-value prev-prev)))
                   (eq types:*ws* (token:xyz-get-type-value prev)))
              (xyz-replace-token state prev "")
              (pop (parse-state-xyz-out-list state)))

        (xyz-add-to-out-list state token)
        (pop (parse-state-xyz-opens state))
        (pop (parse-state-indent state))))


(defun xyz-is-loop-key (state token)
    (let ((prev (car (parse-state-xyz-seen state)))
          (key (if (token:xyz-is-type types:*colons* token)
                   (token:xyz-get-text (xyz-next-next-token state))
                   (token:xyz-get-text token))))

        (and (token:xyz-is-type types:*ws* prev)
             (member (string-downcase key) *loop-keys* :test #'string=))))


(defun xyz-process-token (state token)
    (let ((prev (car (parse-state-xyz-seen state))))

        (when prev
              (cond ((or (token:xyz-is-type types:*line-comment* token)
                         (token:xyz-is-type types:*block-comment* token))
                        (if (and (token:xyz-is-type types:*ws* prev)
                                 (not (xyz-out-of-range (parse-state-range state) prev))
                                 (xyz-same-line prev token))
                            (when (not (string-equal " " (token:xyz-get-text prev)))
                                  (xyz-replace-token state prev " "))
                            (xyz-fix-indent state)))

                    ((token:xyz-is-type types:*ws* prev) (xyz-fix-indent state))

                    ((and (not (token:xyz-is-type types:*colons* token))
                          (xyz-need-space-p prev))
                        (insert-text state (token:xyz-get-end prev) " "))))

        (xyz-add-to-out-list state token)

        (xyz-update-aligned state)))


(defun xyz-check-end-space (state)
    (let* ((token (car (parse-state-xyz-seen state)))
           (nl-count (min 1
                         (if token
                             (the fixnum (xyz-new-line-count token))
                             0)))
           (str (indent-string nl-count 0)))

        (when (and token
                   (not (xyz-out-of-range (parse-state-range state) token))
                   (token:xyz-is-type types:*ws* token))
              (xyz-replace-token state token str))))


(defun update-options (state opts)
    (when (assoc :indent-width opts)
          (setf (options-indent-width (parse-state-options state))
              (cdr (assoc :indent-width opts)))))


(defun xyz-is-body-next (state)
    (let* ((token (car (parse-state-xyz-opens state)))
           (lambda-list (token:get-lambda-list token)))
        (and token
             (not (eq 'cons (type-of (car lambda-list))))
             (or (string= (the symbol (car lambda-list)) "&BODY")
                 (string= (the symbol (car lambda-list)) "&REST")))))


(defun xyz-do-step (state)
    (let* ((token (xyz-next-token state))
           (form-open (car (parse-state-xyz-opens state)))
           (lambda-list (token:get-lambda-list form-open)))

        (when (and form-open
                   lambda-list
                   (not (token:xyz-is-type types:*ws* token)))
              (when (xyz-is-body-next state)
                    (pop-next-indent state))
              (token:set-lambda-list form-open (cdr lambda-list)))

        (cond ((token:xyz-is-type types:*open-paren* token) (xyz-process-open state token))
              ((token:xyz-is-type types:*close-paren* token) (xyz-process-close state token))
              ((token:xyz-is-type types:*ws* token) nil)
              (T (xyz-process-token state token)))

        (push token (parse-state-xyz-seen state))
        (xyz-pop-token state)))


(defun range (input range &optional opts)
    (let* ((tokens (tokenizer:xyz-from-stream input))
           (state (make-parse-state :xyz-tokens tokens
                                    :range range
                                    :cur-pkg (package-name *package*))))

        (when opts
              (update-options state opts))

        (loop :while (parse-state-xyz-tokens state)

              :do (xyz-do-step state)

              :finally (progn (xyz-check-end-space state)
                              (return (reverse (parse-state-edits state)))))))


(defun xyz-get-on-type-indent (state)
    (when (xyz-is-body-next state)
          (pop-next-indent state))

    (get-next-indent state))


(defun on-type (input &key options pos)
    (let* ((tokens (tokenizer:xyz-from-stream input))
           (state (make-parse-state :xyz-tokens tokens
                                    :range (range:create (pos:create 0 0) pos)
                                    :cur-pkg (package-name *package*))))

        (when options
              (update-options state options))

        (when tokens
              (loop :for token := (xyz-next-token state)
                    :for token-end := (token:xyz-get-end token)

                    :while (and token
                                (pos:less-or-equal token-end pos))

                    :do (xyz-do-step state)

                    :finally (let* ((indent (if token
                                                (xyz-get-on-type-indent state)
                                                0))
                                    (line (pos:line pos))
                                    (new-range (range:create (pos:create line 0) pos)))

                                 (return (list (edit:create :range new-range
                                                            :text (indent-string 0 indent)))))))))