(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :add-token
             :create
             :get-end
             :xyz-get-end
             :get-end-offset
             :xyz-get-end-offset
             :get-form-type
             :get-kids
             :get-package
             :get-start
             :xyz-get-start
             :get-start-offset
             :xyz-get-start-offset
             :get-sym-text
             :xyz-get-sym-text
             :get-tokens
             :set-package)
    (:local-nicknames (:pos :alive/position)
                      (:token :alive/parse/token)))

(in-package :alive/parse/form)


(defstruct form
    kids
    kind
    tokens
    (pkg "cl-user"))


(defun add-kid (form kid)
    (let* ((rev-kids (reverse (form-kids form))))
        (setf (form-kids form) (reverse (push kid rev-kids)))))


(defun add-token (form token)
    (let* ((rev-tokens (reverse (form-tokens form))))
        (setf (form-tokens form) (reverse (push token rev-tokens)))))


(defun get-end (form)
    (when (form-p form)
          (let* ((token (car (reverse (form-tokens form))))
                 (token-end (or (token:get-end token) (pos:create 0 0)))
                 (kid (car (reverse (form-kids form))))
                 (kid-end (or (get-end kid) (pos:create 0 0))))
              (if (pos:less-than token-end kid-end)
                  kid-end
                  token-end))))


(defun xyz-get-end (form)
    (when (form-p form)
          (let* ((token (car (reverse (form-tokens form))))
                 (token-end (or (token:xyz-get-end token) (pos:create 0 0)))
                 (kid (car (reverse (form-kids form))))
                 (kid-end (or (xyz-get-end kid) (pos:create 0 0))))
              (if (pos:less-than token-end kid-end)
                  kid-end
                  token-end))))


(defun get-end-offset (form)
    (when (form-p form)
          (let* ((token (car (reverse (form-tokens form))))
                 (token-end (or (token:get-end-offset token) 0))
                 (kid (car (reverse (form-kids form))))
                 (kid-end (or (get-end-offset kid) 0)))
              (max token-end kid-end))))


(defun xyz-get-end-offset (form)
    (when (form-p form)
          (let* ((token (car (reverse (form-tokens form))))
                 (token-end (or (token:xyz-get-end-offset token) 0))
                 (kid (car (reverse (form-kids form))))
                 (kid-end (or (xyz-get-end-offset kid) 0)))
              (max token-end kid-end))))


(defun get-kids (form)
    (when (form-p form)
          (form-kids form)))


(defun get-tokens (form)
    (when (form-p form)
          (form-tokens form)))


(defun get-sym-text (form)
    (when (and (form-p form)
               (eq (form-kind form) alive/types:*symbol*))
          (apply #'concatenate 'string
              (mapcar 'token:get-text (get-tokens (second (get-kids form)))))))


(defun xyz-get-sym-text (form)
    (when (and (form-p form)
               (eq (form-kind form) alive/types:*symbol*))
          (apply #'concatenate 'string
              (mapcar 'token:xyz-get-text (get-tokens form)))))


(defun get-start (form)
    (when (form-p form)
          (let ((token (car (form-tokens form))))
              (token:get-start token))))


(defun xyz-get-start (form)
    (when (form-p form)
          (let ((token (car (form-tokens form))))
              (token:xyz-get-start token))))


(defun get-start-offset (form)
    (when (form-p form)
          (let ((token (car (form-tokens form))))
              (token:get-start-offset token))))


(defun xyz-get-start-offset (form)
    (when (form-p form)
          (let ((token (car (form-tokens form))))
              (token:xyz-get-start-offset token))))


(defun get-form-type (form)
    (when (form-p form)
          (form-kind form)))


(defun get-package (form)
    (when (form-p form)
          (form-pkg form)))


(defun set-package (form pkg)
    (when (form-p form)
          (setf (form-pkg form) pkg)))


(defun create (&key form-type kids tokens pkg)
    (make-form :kind form-type
               :kids kids
               :tokens tokens
               :pkg pkg))
