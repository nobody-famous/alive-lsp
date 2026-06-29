(defpackage :alive/parse/form
    (:use :cl)
    (:export :xyz-add-kid
             :xyz-add-token
             :xyz-create
             :xyz-get-end
             :xyz-get-end-offset
             :xyz-get-kids
             :xyz-get-start
             :xyz-get-start-offset
             :xyz-get-form-type
             :xyz-get-tokens)
    (:local-nicknames (:pos :alive/position)
                      (:token :alive/parse/token)))

(in-package :alive/parse/form)


(defstruct form
    form-type
    kids
    tokens)


(defun xyz-add-kid (form kid)
    (let* ((rev-kids (reverse (form-kids form))))
        (setf (form-kids form) (reverse (push kid rev-kids)))))


(defun xyz-add-token (form token)
    (let* ((rev-tokens (reverse (form-tokens form))))
        (setf (form-tokens form) (reverse (push token rev-tokens)))))


(defun xyz-get-end (form)
    (when form
          (let* ((token (car (reverse (form-tokens form))))
                 (token-end (or (token:get-end token) (pos:create 0 0)))
                 (kid (car (reverse (form-kids form))))
                 (kid-end (or (xyz-get-end kid) (pos:create 0 0))))
              (if (pos:less-than token-end kid-end)
                  kid-end
                  token-end))))


(defun xyz-get-end-offset (form)
    (when form
          (let* ((token (car (reverse (form-tokens form))))
                 (token-end (or (token:get-end-offset token) 0))
                 (kid (car (reverse (form-kids form))))
                 (kid-end (or (xyz-get-end-offset kid) 0)))
              (max token-end kid-end))))


(defun xyz-get-kids (form)
    (when form
          (form-kids form)))


(defun xyz-get-tokens (form)
    (when form
          (form-tokens form)))


(defun xyz-get-start (form)
    (when form
          (let ((token (car (form-tokens form))))
              (token:get-start token))))


(defun xyz-get-start-offset (form)
    (when form
          (let ((token (car (form-tokens form))))
              (token:get-start-offset token))))


(defun xyz-get-form-type (form)
    (when form
          (form-form-type form)))


(defun xyz-create (&key form-type kids tokens)
    (make-form :form-type form-type
               :kids kids
               :tokens tokens))
