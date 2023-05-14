(defpackage :alive/format
    (:use :cl)
    (:export :on-type
             :eol
             :range)
    (:local-nicknames (:edit :alive/text-edit)
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

(defparameter *start-form* 100)

(defparameter *always* 0)
(defparameter *never* 1)
(defparameter *multiline* 2)


(defstruct options
    (indent-width 2)
    (max-blank-lines 2)
    (parens-own-line *never*))


(defun create-start-form ()
    (let ((form (make-hash-table :test #'equalp)))

        (setf (gethash "start" form) nil)
        (setf (gethash "end" form) nil)
        (setf (gethash "aligned" form) nil)
        (setf (gethash "isCond" form) nil)
        (setf (gethash "isLoop" form) nil)
        (setf (gethash "isLoopIndent" form) nil)
        (setf (gethash "lambdaList" form) nil)
        (setf (gethash "isMultiline" form) nil)

        (setf (gethash "typeValue" form) *start-form*)
        (setf (gethash "text" form) "(")

        form))


(defun new-start-form (token)
    (let ((form (create-start-form)))

        (setf (gethash "start" form) (token:get-start token))
        (setf (gethash "end" form) (token:get-end token))

        form))


(defun same-line (token1 token2)
    (and token1
         token2
         (= (the fixnum (pos:line (token:get-start token1)))
             (the fixnum (pos:line (token:get-start token2))))))


(defstruct parse-state
    tokens
    range
    (indent (list 0))
    edits
    out-list
    seen
    opens
    cur-pkg
    (options (make-options)))


(defun next-token (state)
    (car (parse-state-tokens state)))


(defun next-next-token (state)
    (cadr (parse-state-tokens state)))


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


(defun replace-token (state token text)
    (declare (optimize (speed 0)))

    (let* ((range (range:create (token:get-start token)
                                (token:get-end token)))
           (edit (edit:create :range range
                              :text text)))

        (unless (string= text (token:get-text token))
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
            (if indent indent 0))))


(defun pop-next-indent (state)
    (let ((indent (car (parse-state-indent state))))
        (if (eq 'cons (type-of indent))
            (replace-indent state (cdr indent))
            (pop (parse-state-indent state)))))


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


(defun set-cur-pkg (state)
    (let ((token1 (car (parse-state-tokens state)))
          (token2 (cadr (parse-state-tokens state))))
        (cond ((and (token:is-type types:*colons* token1)
                    (token:is-type types:*symbol* token2))
                  (setf (parse-state-cur-pkg state)
                      (format nil "~A~A"
                          (token:get-text token1)
                          (token:get-text token2))))
              ((token:is-type types:*macro* token1)
                  (setf (parse-state-cur-pkg state)
                      (token:get-text token1))))))


(defun lookup-lambda-list (token1 token2 token3)
    (cond ((and (token:is-type types:*symbol* token1)
                (token:is-type types:*colons* token2)
                (token:is-type types:*symbol* token3))

              (symbols:get-lambda-list (token:get-text token3)
                                       (token:get-text token1)))

          ((and (token:is-type types:*symbol* token1)
                (not (token:is-type types:*colons* token2)))

              (symbols:get-lambda-list (token:get-text token1)
                                       (package-name *package*)))))


(defun force-aligned-p (token)
    (let ((name (string-downcase (token:get-text token))))

        (member name *force-align-targets* :test #'string=)))


(defun token-is (token text)
    (declare (type simple-string text))

    (string-equal text (token:get-text token)))


(defun align-first-item (state token form-open prev-open)
    (let* ((ns (car (parse-state-tokens state)))
           (colons (cadr (parse-state-tokens state)))
           (sym (caddr (parse-state-tokens state)))
           (lambda-list (lookup-lambda-list ns colons sym)))

        (when (string-equal "in-package" (token:get-text token))
              (setf (parse-state-cur-pkg state) NIL))

        (cond ((token-is token "cond") (setf (gethash "isCond" form-open) T)
                                       (replace-indent state (pos:col (token:get-start token))))

              ((token-is token "loop") (setf (gethash "isLoop" form-open) T)
                                       (replace-indent state (pos:col (token:get-start token))))

              ((and prev-open (gethash "isCond" prev-open))
                  (replace-indent state (the fixnum (+ (the fixnum (options-indent-width (parse-state-options state)))
                                                       (the fixnum (pos:col (token:get-start token)))
                                                       (the fixnum -1)))))

              ((force-aligned-p token) (replace-indent state (pos:col (token:get-start token))))

              ((has-body lambda-list) (setf (gethash "aligned" form-open) T)
                                      (setf (gethash "lambdaList" form-open) lambda-list)
                                      (replace-indent state (cons (the fixnum (+ (the fixnum (* 2
                                                                                                (the fixnum (options-indent-width (parse-state-options state)))))
                                                                                 (the fixnum (pos:col (token:get-start token)))
                                                                                 (the fixnum -1)))
                                                                  (the fixnum (+ (the fixnum (options-indent-width (parse-state-options state)))
                                                                                 (the fixnum (pos:col (token:get-start token)))
                                                                                 (the fixnum -1))))))

              (lambda-list (replace-indent state (the fixnum (+ (the fixnum (options-indent-width (parse-state-options state)))
                                                                (the fixnum (pos:col (token:get-start token)))
                                                                (the fixnum -1)))))

              (T (replace-indent state (pos:col (token:get-start token)))))))


(defun align-next-item (state token form-open)
    (cond ((and (token:is-type types:*symbol* (car (parse-state-seen state)))
                (token:is-type types:*colons* token)
                (token:is-type types:*symbol* (cadr (parse-state-tokens state))))
              (add-to-out-list state (cadr (parse-state-tokens state)))
              (pop-token state))

          ((and (token:is-type types:*colons* (car (parse-state-seen state)))
                (token:is-type types:*symbol* token))
              NIL)

          (T (unless (parse-state-cur-pkg state)
                 (set-cur-pkg state))
             (setf (gethash "aligned" form-open) T)
             (replace-indent state (pos:col (token:get-start token))))))


(defun update-aligned (state)
    (let* ((form-open (car (parse-state-opens state)))
           (prev-open (cadr (parse-state-opens state)))
           (token (car (parse-state-out-list state)))
           (prev (cadr (parse-state-out-list state)))
           (pkg (packages:for-string (parse-state-cur-pkg state)))
           (*package* (if pkg pkg *package*)))

        (cond ((prev-is-start-form state)
                  (align-first-item state token form-open prev-open))

              ((and form-open
                    (gethash "isLoop" form-open)
                    (not (gethash "aligned" form-open))
                    (is-loop-key state token)
                    (token:is-type types:*ws* prev))
                  (when (or (not (gethash "isLoopIndent" form-open))
                            (token:is-multiline prev))
                        (setf (gethash "aligned" form-open) T)
                        (setf (gethash "loopIndent" form-open) T)
                        (replace-indent state (the fixnum (+ (the fixnum (options-indent-width (parse-state-options state)))
                                                             (the fixnum (pos:col (token:get-start token))))))))

              ((and form-open
                    (not (gethash "aligned" form-open)))
                  (align-next-item state token form-open)))))


(defun fix-indent (state)
    (let* ((indent (get-next-indent state))
           (token (car (parse-state-seen state)))
           (prev (cadr (parse-state-seen state)))
           (next (next-token state))
           (nl-count (if (zerop (list-length (parse-state-opens state)))
                         (min (new-line-count token) 3)
                         (min (new-line-count token) 2)))
           (start (token:get-start token))
           (end (token:get-end token)))

        (when (and next
                   (is-loop-key state next)
                   (token:is-multiline token))
              (decf (the fixnum indent)
                    (the fixnum (options-indent-width (parse-state-options state)))))

        (when (token:is-type types:*ws* token)
              (if (out-of-range (parse-state-range state) token)
                  (add-to-out-list state token)
                  (cond ((or (not prev)
                             (token:is-type *start-form* prev))
                            (replace-token state token ""))

                        ((= (the fixnum (pos:line start)) (the fixnum (pos:line end)))
                            (if (string-equal " " (token:get-text token))
                                (add-to-out-list state token)
                                (progn (add-to-out-list state
                                                        (token:create :type-value types:*ws*
                                                                      :start (token:get-start token)
                                                                      :end (pos:create (pos:line start)
                                                                                       (+ (the fixnum 1) (the fixnum (pos:col start))))
                                                                      :text " "))
                                       (replace-token state token " "))))

                        (T (let* ((str (indent-string nl-count indent))
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
        (pop-next-indent state)))


(defun is-loop-key (state token)
    (let ((prev (car (parse-state-seen state)))
          (key (if (token:is-type types:*colons* token)
                   (token:get-text (next-next-token state))
                   (token:get-text token))))

        (and (token:is-type types:*ws* prev)
             (member (string-downcase key) *loop-keys* :test #'string=))))


(defun process-token (state token)
    (let ((prev (car (parse-state-seen state))))

        (when prev
              (cond ((or (token:is-type types:*line-comment* token)
                         (token:is-type types:*block-comment* token))
                        (if (and (token:is-type types:*ws* prev)
                                 (not (out-of-range (parse-state-range state) prev))
                                 (same-line prev token))
                            (when (not (string-equal " " (token:get-text prev)))
                                  (replace-token state prev " "))
                            (fix-indent state)))

                    ((token:is-type types:*ws* prev) (fix-indent state))

                    ((and (not (token:is-type types:*colons* token))
                          (need-space-p prev))
                        (insert-text state (token:get-end prev) " "))))

        (add-to-out-list state token)

        (update-aligned state)))


(defun check-end-space (state)
    (let* ((token (car (parse-state-seen state)))
           (nl-count (min 1
                         (if token
                             (the fixnum (new-line-count token))
                             0)))
           (str (indent-string nl-count 0)))
        (when (and token
                   (not (out-of-range (parse-state-range state) token))
                   (token:is-type types:*ws* token))
              (replace-token state token str))))


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
                             (setf (gethash "isMultiline" (car opens)) T))
                       (push token converted)))

          :finally (return (reverse converted))))


(defun update-options (state opts)
    (when (assoc :indent-width opts)
          (setf (options-indent-width (parse-state-options state))
              (cdr (assoc :indent-width opts)))))


(defun is-body-next (state)
    (let ((form-open (car (parse-state-opens state))))
        (and form-open
             (not (eq 'cons (type-of (car (gethash "lambdalist" form-open)))))
             (or (string= (the symbol (car (gethash "lambdalist" form-open))) "&BODY")
                 (string= (the symbol (car (gethash "lambdalist" form-open))) "&REST")))))


(defun do-step (state)

    (let ((token (next-token state))
          (form-open (car (parse-state-opens state))))

        (when (and form-open
                   (gethash "lambdaList" form-open)
                   (not (token:is-type types:*ws* token)))
              (when (is-body-next state)
                    (pop-next-indent state))
              (pop (gethash "lambdaList" form-open)))

        (cond ((token:is-type *start-form* token) (process-open state token))
              ((token:is-type types:*close-paren* token) (process-close state token))
              ((token:is-type types:*ws* token) nil)
              (T (process-token state token)))

        (push token (parse-state-seen state))
        (pop-token state)))


(defun range (input range &optional opts)
    (let* ((tokens (convert-tokens (tokenizer:from-stream input)))
           (state (make-parse-state :tokens tokens
                                    :range range
                                    :cur-pkg (package-name *package*))))

        (when opts
              (update-options state opts))

        (loop :while (parse-state-tokens state)

              :do (do-step state)

              :finally (progn (check-end-space state)
                              (return (reverse (parse-state-edits state)))))))


(defun get-on-type-indent (state)
    (when (is-body-next state)
          (pop-next-indent state))

    (get-next-indent state))


(defun on-type (input &key options pos)
    (let* ((tokens (convert-tokens (tokenizer:from-stream input)))
           (state (make-parse-state :tokens tokens
                                    :range (range:create (pos:create 0 0) pos)
                                    :cur-pkg (package-name *package*))))

        (when options
              (update-options state options))

        (when tokens
              (loop :for token := (next-token state)
                    :for token-end := (token:get-end token)

                    :while (and token
                                (pos:less-or-equal token-end pos))

                    :do (do-step state)

                    :finally (let* ((indent (if token
                                                (get-on-type-indent state)
                                                0))
                                    (line (cdr (assoc :line pos)))
                                    (new-range (range:create (pos:create line 0) pos)))

                                 (return (list (edit:create :range new-range
                                                            :text (indent-string 0 indent)))))))))