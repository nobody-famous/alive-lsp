(defpackage :alive/lsp/sem-analysis
    (:use :cl)
    (:export :to-sem-tokens)
    (:local-nicknames (:pos :alive/parse/pos)
                      (:sem-types :alive/lsp/types/sem-tokens)
                      (:token :alive/parse/token)
                      (:types :alive/types)))

(in-package :alive/lsp/sem-analysis)


(defclass sem-token ()
    ((token-type :accessor token-type
                 :initform nil
                 :initarg :token-type)
     (line :accessor line
           :initform nil
           :initarg :line)
     (start-col :accessor start-col
                :initform nil
                :initarg :start-col)
     (end-col :accessor end-col
              :initform nil
              :initarg :end-col)))


(defmethod print-object ((obj sem-token) out)
    (format out "{~A line ~A start ~A end ~A}"
            (token-type obj)
            (line obj)
            (start-col obj)
            (end-col obj)))


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
    (when (eq types:*ws*
              (token:type-value (peek-token state)))
          (next-token state)))


(defun is-type (token target)
    (eq (token:type-value token)
        target))


(defun is-next-type (state target)
    (eq (token:type-value (peek-token state))
        target))


(defun add-sem-token (state token sem-type)
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
          :for new-token := (make-instance 'sem-token
                                           :line line
                                           :start-col start-col
                                           :end-col end-col
                                           :token-type new-type) :do
              (push new-token (sem-tokens state))))


(defun process-expr (state)
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

              ((is-type token types:*symbol*) (if (is-next-type state types:*colons*)
                                                  (progn (add-sem-token state token sem-types:*namespace*)

                                                         (setf token (next-token state))

                                                         (add-sem-token state token sem-types:*symbol*)
                                                         (process-expr state))
                                                  (add-sem-token state token sem-types:*symbol*)))

              ((is-type token types:*ws*) nil)

              ((forced-type state) (add-sem-token state
                                                  token
                                                  (forced-type state)))

              (T (error (format nil "Unhandled token: ~A" token))))))


(defun to-sem-tokens (tokens)
    (loop :with state := (make-instance 'analysis-state :lex-tokens tokens)

          :while (lex-tokens state)
          :do (process-expr state)

          :finally (return (reverse (sem-tokens state)))))
