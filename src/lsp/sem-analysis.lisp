(defpackage :alive/lsp/sem-analysis
    (:use :cl)
    (:export :to-sem-tokens)
    (:local-nicknames (:pos :alive/parse/pos)
                      (:sem-types :alive/lsp/types/sem-tokens)
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
            (sem-tokens obj)
            (opens obj)))


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


(defun is-keyword (token)
    (find-symbol (string-upcase (token:text token)) :common-lisp))


(defun is-number (token)
    (loop :with is-valid := T
          :with have-decimal := nil
          :with have-after-decimal := nil
          :with have-div := nil
          :with have-after-div := nil

          :for ch :across (token:text token) :do
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
    (labels ((process-list (state paren-token)
                  (add-sem-token state paren-token sem-types:*parenthesis*)
                  (skip-ws state)

                  (when (is-next-type state types:*symbol*)
                        (format T "SYMBOL ~A~%" (token:text (peek-token state))))

                  (loop :for token := (peek-token state)

                        :until (or (not token)
                                   (is-type token types:*close-paren*))
                        :do (process-expr state)

                        :finally (progn
                                  (add-sem-token state token sem-types:*parenthesis*)
                                  (next-token state))))

             (get-symbol-type (token)
                  (cond ((is-keyword token) sem-types:*keyword*)
                        ((is-number token) sem-types:*number*)
                        (t sem-types:*symbol*)))

             (process-symbol (state symbol-token)
                  (add-sem-token state symbol-token (get-symbol-type symbol-token))))

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

                  ((is-type token types:*symbol*) (if (is-next-type state types:*colons*)
                                                      (progn (add-sem-token state token sem-types:*namespace*)

                                                             (setf token (next-token state))

                                                             (add-sem-token state token sem-types:*symbol*)
                                                             (process-expr state))
                                                      (process-symbol state token)))

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
