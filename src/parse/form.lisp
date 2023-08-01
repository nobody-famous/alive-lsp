(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :create
             :get-end
             :get-end-offset
             :get-kids
             :get-start
             :get-start-offset
             :get-form-type
             :get-in-pkg
             :is-in-pkg
             :set-end
             :set-end-offset
             :set-is-in-pkg
             :to-string))

(in-package :alive/parse/form)


(defun add-kid (form kid)
    (let* ((rev-kids (reverse (gethash "kids" form))))
        (setf (gethash "kids" form) (reverse (push kid rev-kids)))))


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


(defun get-end-offset (form)
    (when form
          (gethash "endOffset" form)))


(defun get-kids (form)
    (when form
          (gethash "kids" form)))


(defun get-start (form)
    (when form
          (gethash "start" form)))


(defun get-start-offset (form)
    (when form
          (gethash "startOffset" form)))


(defun get-form-type (form)
    (when form
          (gethash "formType" form)))


(defun create (&key start start-offset end end-offset form-type in-pkg kids)
    (let ((form (make-hash-table :test #'equalp)))

        (setf (gethash "start" form) start)
        (setf (gethash "startOffset" form) start-offset)
        (setf (gethash "end" form) end)
        (setf (gethash "endOffset" form) end-offset)
        (setf (gethash "formType" form) form-type)
        (setf (gethash "inPkg" form) in-pkg)
        (setf (gethash "kids" form) kids)

        form))
