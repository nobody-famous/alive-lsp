(defpackage :alive/lsp/sem-analysis
    (:use :cl)
    (:export :to-sem-tokens)
    (:local-nicknames (:pos :alive/position)
                      (:sem-types :alive/lsp/types/sem-tokens)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:types :alive/types)))

(in-package :alive/lsp/sem-analysis)


(defclass analysis-state ()
    ((lex-tokens :accessor lex-tokens
                 :initform nil
                 :initarg :lex-tokens)
     (sem-tokens :accessor sem-tokens
                 :initform nil
                 :initarg :sem-tokens)
     (forced-type :accessor forced-type
                  :initform nil
                  :initarg :forced-type)
     (opens :accessor opens
            :initform nil
            :initarg :opens)))


(defmethod print-object ((obj analysis-state) out)
    (format out "{lex ~A sem ~A opens ~A}"
            (lex-tokens obj)
            (reverse (sem-tokens obj))
            (opens obj)))


(defclass symbol-with-pkg ()
    ((pkg :accessor pkg
          :initform nil
          :initarg :pkg)
     (colons :accessor colons
             :initform nil
             :initarg :colons)
     (sym :accessor sym
          :initform nil
          :initarg :sym)))


(defmethod print-object ((obj symbol-with-pkg) out)
    (format out "{pkg: ~A; colons: ~A; sym: ~A}"
            (pkg obj)
            (colons obj)
            (sym obj)))


(defun peek-token (state)
    (car (lex-tokens state)))


(defun eat-token (state)
    (setf (lex-tokens state) (cdr (lex-tokens state))))


(defun next-token (state)
    (let ((token (car (lex-tokens state))))

        (eat-token state)
        token))


(defun skip-ws (state)
    (when (and (peek-token state)
               (eq types:*ws* (token:get-type-value (peek-token state))))

          (eat-token state)))


(defun is-type (token target)
    (and token
         (eq (token:get-type-value token)
             target)))


(defun is-next-type (state target)
    (let ((peeked (peek-token state)))
        (and peeked
             (eq (token:get-type-value peeked) target))))


(defun add-sem-token (state token sem-type)
    (when token
          (loop :with new-type := (or (forced-type state)
                                      sem-type)
                :with start := (token:start token)
                :with end := (token:end token)

                :for line :from (pos:line start) :to (pos:line end)
                :for start-col := (if (eq line (pos:line start))
                                      (pos:col start)
                                      0)
                :for end-col := (if (eq line (pos:line end))
                                    (pos:col end)
                                    #xFFFFFFFF)
                :for new-token := (sem-types:create :line line
                                                    :start start-col
                                                    :end end-col
                                                    :token-type new-type) :do
                    (push new-token (sem-tokens state)))))


(defun is-number (text)
    (loop :with is-valid := T
          :with have-decimal := nil
          :with have-after-decimal := nil
          :with have-div := nil
          :with have-after-div := nil

          :for ch :across text :do
              (cond ((char= ch #\.) (if have-decimal
                                        (setf is-valid nil)
                                        (setf have-decimal T)))
                    ((char= ch #\/) (if have-div
                                        (setf is-valid nil)
                                        (setf have-div T)))
                    ((digit-char-p ch) (when (and is-valid
                                                  have-div)
                                             (setf have-after-div T))
                                       (when (and is-valid
                                                  have-decimal)
                                             (setf have-after-decimal T)))
                    (T (setf is-valid nil)))

          :finally (return (and is-valid
                                (eq have-div have-after-div)
                                (eq have-decimal have-after-decimal)))))


(defun process-expr (state)
    (labels ((finish-list (state)
                  (skip-ws state)
                  (loop :for token := (progn (skip-ws state)
                                             (peek-token state))

                        :until (or (not token)
                                   (is-type token types:*close-paren*))
                        :do (process-expr state)))

             (process-nested-param (state)
                  (add-sem-token state (peek-token state) sem-types:*parenthesis*)
                  (next-token state)
                  (skip-ws state)

                  (when (is-type (peek-token state) types:*symbol*)
                        (add-sem-token state (peek-token state) sem-types:*parameter*)
                        (next-token state)
                        (skip-ws state))

                  (finish-list state)
                  (when (is-type (peek-token state) types:*close-paren*)
                        (add-sem-token state (peek-token state) sem-types:*parenthesis*)
                        (next-token state)
                        (skip-ws state)))

             (process-param (state)
                  (skip-ws state)

                  (let ((token (peek-token state)))
                      (cond ((is-type token types:*symbol*) (add-sem-token state token sem-types:*parameter*)
                                                            (next-token state))
                            ((is-type token types:*open-paren*) (process-nested-param state))
                            ((is-type token types:*ifdef-false*) (process-expr state))
                            ((is-type token types:*ifdef-true*) (next-token state))
                            (T (format T "UNHANDLED PARAM ~A~%" token)
                               (next-token state)))))

             (process-lambda-list (state)
                  (if (is-type (peek-token state) types:*open-paren*)
                      (progn (add-sem-token state (peek-token state) sem-types:*parenthesis*)
                             (next-token state)

                             (loop :for token := (progn (skip-ws state)
                                                        (peek-token state))

                                   :until (or (not token)
                                              (is-type token types:*close-paren*))

                                   :do (process-param state)
                                       (skip-ws state)

                                   :finally (progn (add-sem-token state token sem-types:*parenthesis*)
                                                   (next-token state))))
                      (process-expr state)))

             (process-fn (state obj)
                  (skip-ws state)

                  (loop :with fn-name := (if (sym obj) (token:get-text (sym obj)) nil)
                        :with pkg-name := (if (pkg obj) (token:get-text (pkg obj)) nil)
                        :with done := nil

                        :for item :in (symbols:get-lambda-list fn-name pkg-name)
                        :for item-token := (peek-token state)

                        :until (or done
                                   (not item-token)
                                   (is-type item-token types:*close-paren*))

                        :do (cond ((equal (type-of item) 'cons) (process-list state))

                                  ((char= #\& (char (string item) 0)) (finish-list state)
                                                                      (setf done T))

                                  ((char= #\( (char (string item) 0)) (process-list state))

                                  ((string= "LAMBDA-LIST" (string item)) (process-lambda-list state))

                                  ((and (string= "BINDINGS" (string item))
                                        (is-type item-token types:*open-paren*)) (process-list state))

                                  (t (process-expr state)))

                            (skip-ws state)

                        :finally (progn (when (is-type item-token types:*close-paren*)
                                              (add-sem-token state item-token sem-types:*parenthesis*)
                                              (next-token state)))))

             (process-list (state)
                  (add-sem-token state (peek-token state) sem-types:*parenthesis*)
                  (next-token state)
                  (skip-ws state)

                  (let ((token (peek-token state)))
                      (cond ((is-type token types:*close-paren*) (next-token state)
                                                                 (add-sem-token state token sem-types:*parenthesis*))

                            ((or (is-type token types:*symbol*)
                                 (is-type token types:*colons*))
                             (let* ((obj (get-symbol-pkg state token))
                                    (pkg-name (if (pkg obj)
                                                  (token:get-text (pkg obj))
                                                  (package-name *package*)))
                                    (sym-name (if (sym obj)
                                                  (token:get-text (sym obj))
                                                  nil)))

                                 (if (symbols:callable-p sym-name pkg-name)
                                     (progn (process-symbol state obj (cond ((and (colons obj)
                                                                                  (not (pkg obj))) sem-types:*symbol*)
                                                                            ((symbols:function-p sym-name pkg-name) sem-types:*function*)
                                                                            ((symbols:macro-p sym-name pkg-name) sem-types:*macro*)
                                                                            (T sem-types:*keyword*)))
                                            (process-fn state obj))
                                     (process-symbol state obj))))

                            (t (process-expr state)))))

             (get-symbol-type (token has-colons &optional pkg-name)
                  (cond ((string= (string-downcase (token:get-text token)) "nil") sem-types:*symbol*)
                        ((string= (string-downcase (token:get-text token)) "t") sem-types:*symbol*)
                        ((is-number (token:get-text token)) sem-types:*number*)
                        ((symbols:function-p (token:get-text token) pkg-name) sem-types:*function*)
                        ((symbols:macro-p (token:get-text token) pkg-name) sem-types:*macro*)
                        (t (when has-colons sem-types:*symbol*))))

             (get-symbol-pkg (state token)
                  (cond ((is-type (peek-token state) types:*colons*)
                         (let ((token1 (next-token state)))
                             (let ((after (peek-token state)))
                                 (if (is-type after types:*symbol*)
                                     (progn (next-token state)
                                            (make-instance 'symbol-with-pkg
                                                           :pkg nil
                                                           :colons token1
                                                           :sym after))
                                     (make-instance 'symbol-with-pkg
                                                    :pkg nil
                                                    :colons token1
                                                    :sym nil)))))

                        ((is-type (peek-token state) types:*symbol*)
                         (let ((token1 (next-token state)))
                             (if (is-type (peek-token state) types:*colons*)
                                 (let ((token2 (next-token state)))
                                     (if (is-type (peek-token state) types:*symbol*)
                                         (make-instance 'symbol-with-pkg
                                                        :pkg token1
                                                        :colons token2
                                                        :sym (next-token state))
                                         (make-instance 'symbol-with-pkg
                                                        :pkg token1
                                                        :colons token2
                                                        :sym nil)))

                                 (make-instance 'symbol-with-pkg
                                                :pkg nil
                                                :sym token1))))

                        (T (make-instance 'symbol-with-pkg
                                          :pkg nil
                                          :sym token))))

             (process-symbol (state obj &optional (sym-type nil))
                  (if (colons obj)
                      (progn (add-sem-token state (pkg obj) sem-types:*namespace*)
                             (add-sem-token state (colons obj) sem-types:*symbol*)
                             (when (sym obj)
                                   (add-sem-token state
                                                  (sym obj)
                                                  (if sym-type
                                                      sym-type
                                                      (get-symbol-type (sym obj) T)))))

                      (let ((target-type (if sym-type
                                             sym-type
                                             (get-symbol-type (sym obj) NIL))))

                          (when (or (forced-type state)
                                    target-type)
                                (add-sem-token state (sym obj) (if sym-type
                                                                   sym-type
                                                                   (get-symbol-type (sym obj) NIL))))))))

        (let ((token (peek-token state)))
            (cond ((is-type token types:*comment*) (next-token state)
                                                   (add-sem-token state token sem-types:*comment*))

                  ((is-type token types:*string*) (next-token state)
                                                  (add-sem-token state token sem-types:*string*))

                  ((is-type token types:*macro*) (next-token state)
                                                 (add-sem-token state token sem-types:*macro*))

                  ((is-type token types:*ifdef-true*) (next-token state)
                                                      (add-sem-token state token sem-types:*macro*))

                  ((is-type token types:*ifdef-false*) (next-token state)
                                                       (add-sem-token state token sem-types:*comment*)
                                                       (setf (forced-type state) types:*comment*)
                                                       (skip-ws state)
                                                       (process-expr state)
                                                       (setf (forced-type state) nil))

                  ((or (is-type token types:*colons*)
                       (is-type token types:*symbol*)) (process-symbol state
                                                                       (get-symbol-pkg state token)))

                  ((is-type token types:*open-paren*) (process-list state))

                  ((is-type token types:*close-paren*) (next-token state)
                                                       (add-sem-token state token sem-types:*parenthesis*))

                  ((is-type token types:*ws*) (next-token state))

                  ((forced-type state) (next-token state)
                                       (add-sem-token state
                                                      token
                                                      (forced-type state)))

                  (T nil)))))


(defun to-sem-tokens (tokens)
    (loop :with state := (make-instance 'analysis-state :lex-tokens tokens)

          :while (lex-tokens state)
          :do (process-expr state)

          :finally (return (reverse (sem-tokens state)))))
