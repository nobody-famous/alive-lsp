(defpackage :alive/parse/token
    (:use :cl)
    (:export :aligned-p
             :cond-p
             :create
             :get-end
             :get-end-offset
             :get-lambda-list
             :get-start
             :get-start-offset
             :get-text
             :get-type-value
             :is-multiline
             :is-type
             :loop-p
             :set-aligned
             :set-is-cond
             :set-lambda-list
             :set-is-loop
             :set-multiline)
    (:local-nicknames (:pos :alive/position)
                      (:types :alive/types)))

(in-package :alive/parse/token)


(defstruct token
    type-value
    text
    start
    start-offset
    end
    end-offset
    is-aligned
    is-cond
    is-loop
    is-multiline
    lambda-list)


(defun aligned-p (token)
    (when (token-p token)
          (token-is-aligned token)))


(defun cond-p (token)
    (when (token-p token)
          (token-is-cond token)))


(defun loop-p (token)
    (when (token-p token)
          (token-is-loop token)))


(defun set-is-loop (token value)
    (when (token-p token)
          (setf (token-is-loop token) value)))


(defun set-aligned (token value)
    (when (token-p token)
          (setf (token-is-aligned token) value)))


(defun set-is-cond (token value)
    (when (token-p token)
          (setf (token-is-cond token) value)))

(defun set-multiline (token value)
    (when (token-p token)
          (setf (token-is-multiline token) value)))


(defun get-lambda-list (token)
    (when (token-p token)
          (token-lambda-list token)))


(defun set-lambda-list (token value)
    (when (token-p token)
          (setf (token-lambda-list token) value)))


(defun get-type-value (token)
    (when (token-p token)
          (token-type-value token)))


(defun get-text (token)
    (when (token-p token)
          (token-text token)))


(defun get-start (token)
    (when (token-p token)
          (token-start token)))


(defun get-end (token)
    (when (token-p token)
          (token-end token)))


(defun is-type (type token)
    (and (token-p token)
         (= type (get-type-value token))))


(defun is-multiline (token)
    (and (token-p token)
         (not (eq (pos:line (get-start token))
                  (pos:line (get-end token))))))


(defun create (&key type-value start start-offset end end-offset text is-multiline)
    (make-token :type-value type-value
                :start start
                :start-offset start-offset
                :end end
                :end-offset end-offset
                :is-multiline is-multiline
                :text text))
