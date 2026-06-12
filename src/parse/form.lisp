(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :xyz-add-kid
             :add-token
             :xyz-add-token
             :create
             :xyz-create
             :get-end
             :xyz-get-end
             :get-end-offset
             :xyz-get-end-offset
             :get-kids
             :xyz-get-kids
             :get-start
             :xyz-get-start
             :get-start-offset
             :xyz-get-start-offset
             :get-form-type
             :xyz-get-form-type
             :get-in-pkg
             :get-tokens
             :xyz-get-tokens
             :is-in-pkg
             :set-end
             :set-end-offset
             :set-is-in-pkg)
    (:local-nicknames (:token :alive/parse/token)))

(in-package :alive/parse/form)


(defstruct form
    form-type
    kids
    tokens)


(defun add-kid (form kid)
    (let* ((rev-kids (reverse (gethash "kids" form))))
        (setf (gethash "kids" form) (reverse (push kid rev-kids)))))


(defun xyz-add-kid (form kid)
    (let* ((rev-kids (reverse (form-kids form))))
        (setf (form-kids form) (reverse (push kid rev-kids)))))


(defun add-token (form token)
    (let* ((rev-tokens (reverse (gethash "tokens" form))))
        (setf (gethash "tokens" form) (reverse (push token rev-tokens)))))


(defun xyz-add-token (form token)
    (let* ((rev-tokens (reverse (form-tokens form))))
        (setf (form-tokens form) (reverse (push token rev-tokens)))))


(defun set-end (form pos)
    (when form
          (setf (gethash "end" form) pos)))


(defun set-end-offset (form pos)
    (when form
          (setf (gethash "endOffset" form) pos)))


(defun set-is-in-pkg (form value)
    (when form
          (setf (gethash "inPkg" form) value)))


(defun is-in-pkg (form)
    (when form
          (gethash "inPkg" form)))


(defun get-end (form)
    (when form
          (gethash "end" form)))


(defun xyz-get-end (form)
    (when form
          (let ((token (car (reverse (form-tokens form)))))
              (token:get-end token))))


(defun get-end-offset (form)
    (when form
          (gethash "endOffset" form)))


(defun xyz-get-end-offset (form)
    (when form
          (let* ((token (car (reverse (form-tokens form))))
                 (token-end (or (token:get-end-offset token) 0))
                 (kid (car (reverse (form-kids form))))
                 (kid-end (or (xyz-get-end-offset kid) 0)))
              (max token-end kid-end))))


(defun get-kids (form)
    (when form
          (gethash "kids" form)))


(defun xyz-get-kids (form)
    (when form
          (form-kids form)))


(defun get-tokens (form)
    (when form
          (gethash "tokens" form)))


(defun xyz-get-tokens (form)
    (when form
          (form-tokens form)))


(defun get-start (form)
    (when form
          (gethash "start" form)))


(defun xyz-get-start (form)
    (when form
          (let ((token (car (form-tokens form))))
              (token:get-start token))))


(defun get-start-offset (form)
    (when form
          (gethash "startOffset" form)))


(defun xyz-get-start-offset (form)
    (when form
          (let ((token (car (form-tokens form))))
              (token:get-start-offset token))))


(defun get-form-type (form)
    (when form
          (gethash "formType" form)))


(defun xyz-get-form-type (form)
    (when form
          (form-form-type form)))


(defun create (&key start start-offset end end-offset form-type in-pkg kids tokens)
    (let ((form (make-hash-table :test #'equalp)))

        (setf (gethash "start" form) start)
        (setf (gethash "startOffset" form) start-offset)
        (setf (gethash "end" form) end)
        (setf (gethash "endOffset" form) end-offset)
        (setf (gethash "formType" form) form-type)
        (setf (gethash "inPkg" form) in-pkg)
        (setf (gethash "kids" form) kids)
        (setf (gethash "tokens" form) tokens)

        form))


(defun xyz-create (&key form-type kids tokens)
    (make-form :form-type form-type
               :kids kids
               :tokens tokens))
