(defpackage :alive/lsp/sem-analysis
    (:use :cl)
    (:export :to-sem-tokens)
    (:local-nicknames (:pos :alive/parse/pos)
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


(defun next-token (state)
    (let ((token (car (lex-tokens state))))
        (setf (lex-tokens state) (cdr (lex-tokens state)))
        token))


(defun skip-ws (state)
    (when (and (peek-token state)
               (eq types:*ws*
                   (token:type-value (peek-token state))))
          (next-token state)))


(defun is-type (token target)
    (and token
         (eq (token:type-value token)
             target)))


(defun is-next-type (state target)
    (let ((peeked (peek-token state)))
        (and peeked
             (eq (token:type-value peeked) target))))


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


(defun is-keyword (name)
    (if (find-symbol (string-upcase name) :common-lisp)
        T
        nil))


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
    (labels ((process-fn (state obj)
                  (skip-ws state)

                  (loop :with fn-name := (if (sym obj) (token:text (sym obj)) nil)
                        :with pkg-name := (if (pkg obj) (token:text (pkg obj)) nil)

                        :for item :in (symbols:get-lambda-list fn-name pkg-name)
                        :for item-token := (next-token state)

                        :until (or (not item-token)
                                   (is-type item-token types:*close-paren*))

                        :do (skip-ws state)
                            (format T "ITEM ~A ~A ~A ~A ~A~%"
                                    (package-name *package*)
                                    item
                                    (char (string item) 0)
                                    (token:type-value item-token)
                                    (string= "LAMBDA-LIST" (string item)))

                        :finally (when (is-type item-token types:*close-paren*)
                                       (add-sem-token state item-token sem-types:*parenthesis*)
                                       (next-token state))))

             (process-list (state paren-token)
                  (add-sem-token state paren-token sem-types:*parenthesis*)
                  (skip-ws state)

                  (let ((token (next-token state)))
                      (cond ((is-type token types:*close-paren*) (add-sem-token state token sem-types:*parenthesis*))
                            ((is-type token types:*symbol*)
                             (let* ((obj (get-symbol-pkg state token))
                                    (pkg-name (if (pkg obj)
                                                  (token:text (pkg obj))
                                                  (package-name *package*)))
                                    (sym-name (if (sym obj)
                                                  (token:text (sym obj))
                                                  nil)))

                                 (if (symbols:callable-p sym-name pkg-name)
                                     (progn (process-symbol state obj sem-types:*function*)
                                            (process-fn state obj))
                                     (process-symbol state obj))))

                            (t (process-expr state))))

                  #+n (when (is-type (peek-token state) types:*symbol*)
                            (format T "CALLABLE ~A ~A~%"
                                    (peek-token state)
                                    (symbols:callable-p (token:text (peek-token state)))))

                  #+n (loop :for token := (peek-token state)

                            :until (or (not token)
                                       (is-type token types:*close-paren*))
                            :do (process-expr state)

                            :finally (progn
                                      (add-sem-token state token sem-types:*parenthesis*)
                                      (next-token state))))

             (get-symbol-type (token)
                  (cond ((is-keyword (token:text token)) sem-types:*keyword*)
                        ((is-number (token:text token)) sem-types:*number*)
                        (t sem-types:*symbol*)))

             (get-symbol-pkg (state token)
                  (if (is-next-type state types:*colons*)
                      (let ((pkg-token token)
                            (colon-token (next-token state))
                            (after-colon (peek-token state)))
                          (if (is-type after-colon types:*ws*)
                              (make-instance 'symbol-with-pkg
                                             :pkg pkg-token
                                             :colons colon-token
                                             :sym nil)
                              (progn (next-token state)
                                     (make-instance 'symbol-with-pkg
                                                    :pkg pkg-token
                                                    :colons colon-token
                                                    :sym after-colon))))
                      (make-instance 'symbol-with-pkg
                                     :pkg nil
                                     :sym token)))

             (process-symbol (state obj &optional (sym-type nil))
                  (if (colons obj)
                      (progn (add-sem-token state (pkg obj) sem-types:*namespace*)
                             (add-sem-token state (colons obj) sem-types:*symbol*)
                             (when (sym obj)
                                   (add-sem-token state
                                                  (sym obj)
                                                  (if sym-type
                                                      sym-type
                                                      (get-symbol-type (sym obj))))))
                      (add-sem-token state (sym obj) (get-symbol-type (sym obj))))))

        (let ((token (next-token state)))
            (cond ((is-type token types:*comment*) (add-sem-token state token sem-types:*comment*))

                  ((is-type token types:*string*) (add-sem-token state token sem-types:*string*))

                  ((is-type token types:*macro*) (add-sem-token state token sem-types:*macro*))

                  ((is-type token types:*ifdef-true*) (add-sem-token state token sem-types:*macro*))

                  ((is-type token types:*ifdef-false*) (add-sem-token state token sem-types:*comment*)
                                                       (setf (forced-type state) types:*comment*)
                                                       (skip-ws state)
                                                       (process-expr state)
                                                       (setf (forced-type state) nil))

                  ((is-type token types:*colons*) (add-sem-token state token sem-types:*symbol*))

                  ((is-type token types:*symbol*) (process-symbol state
                                                                  (get-symbol-pkg state token)))

                  ((is-type token types:*open-paren*) (process-list state token))

                  ((is-type token types:*close-paren*) (add-sem-token state token sem-types:*parenthesis*))

                  ((is-type token types:*ws*) nil)

                  ((forced-type state) (add-sem-token state
                                                      token
                                                      (forced-type state)))

                  (T nil)))))


(defun to-sem-tokens (tokens)
    (loop :with state := (make-instance 'analysis-state :lex-tokens tokens)

          :while (lex-tokens state)
          :do (process-expr state)

          :finally (return (reverse (sem-tokens state)))))
