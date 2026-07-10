(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :add-token
             :create
             :get-end
             :get-end-offset
             :get-form-type
             :get-kids
             :get-start
             :get-start-offset
             :get-sym-text
             :get-tokens)
    (:local-nicknames (:pos :alive/position)
                      (:token :alive/parse/token)))

(in-package :alive/parse/form)


(defstruct form
    form-type
    kids
    tokens)


(defun add-kid (form kid)
    (let* ((rev-kids (reverse (form-kids form))))
        (setf (form-kids form) (reverse (push kid rev-kids)))))


(defun add-token (form token)
    (let* ((rev-tokens (reverse (form-tokens form))))
        (setf (form-tokens form) (reverse (push token rev-tokens)))))


(defun get-end (form)
    (when form
          (let* ((token (car (reverse (form-tokens form))))
                 (token-end (or (token:get-end token) (pos:create 0 0)))
                 (kid (car (reverse (form-kids form))))
                 (kid-end (or (get-end kid) (pos:create 0 0))))
              (if (pos:less-than token-end kid-end)
                  kid-end
                  token-end))))


(defun get-end-offset (form)
    (when form
          (let* ((token (car (reverse (form-tokens form))))
                 (token-end (or (token:get-end-offset token) 0))
                 (kid (car (reverse (form-kids form))))
                 (kid-end (or (get-end-offset kid) 0)))
              (max token-end kid-end))))


(defun get-kids (form)
    (when form
          (form-kids form)))


(defun get-tokens (form)
    (when form
          (form-tokens form)))


(defun get-sym-text (form)
    (when (and form
               (eq (form-form-type form) alive/types:*symbol*))
          (apply #'concatenate 'string
              (mapcar 'token:get-text (get-tokens (second (get-kids form)))))))


(defun get-start (form)
    (when form
          (let ((token (car (form-tokens form))))
              (token:get-start token))))


(defun get-start-offset (form)
    (when form
          (let ((token (car (form-tokens form))))
              (token:get-start-offset token))))


(defun get-form-type (form)
    (when form
          (form-form-type form)))


(defun create (&key form-type kids tokens)
    (make-form :form-type form-type
               :kids kids
               :tokens tokens))
