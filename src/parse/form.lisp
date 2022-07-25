(defpackage :alive/parse/form
    (:use :cl)
    (:export :add-kid
             :add-kid-new
             :create
             :create-new
             :get-end
             :get-end-new
             :get-end-offset
             :get-end-offset-new
             :get-kids
             :get-kids-new
             :get-start
             :get-start-new
             :get-start-offset
             :get-start-offset-new
             :get-form-type
             :get-form-type-new
             :get-in-pkg
             :get-in-pkg-new
             :is-in-pkg
             :is-in-pkg-new
             :set-end
             :set-end-new
             :set-end-offset
             :set-end-offset-new
             :set-is-in-pkg
             :set-is-in-pkg-new)
    (:local-nicknames (:types :alive/types)))

(in-package :alive/parse/form)


(defclass form ()
        ((start :accessor start
                :initform nil
                :initarg :start)
         (start-offset :accessor start-offset
                       :initform nil
                       :initarg :start-offset)
         (end :accessor end
              :initform nil
              :initarg :end)
         (end-offset :accessor end-offset
                     :initform nil
                     :initarg :end-offset)
         (form-type :accessor form-type
                    :initform nil
                    :initarg :form-type)
         (in-pkg-p :accessor in-pkg-p
                   :initform nil
                   :initarg :in-pkg-p)
         (kids :accessor kids
               :initform nil
               :initarg :kids)))


(defmethod print-object ((obj form) out)
    (format out "{~A:~A(~A:~A) type: ~A; in-pkg-p: ~A; kids:~{~A~}}"
        (start obj)
        (end obj)
        (start-offset obj)
        (end-offset obj)
        (form-type obj)
        (in-pkg-p obj)
        (kids obj)))


(defun add-kid (form kid)
    (let* ((rev-kids (reverse (kids form))))
        (setf (kids form) (reverse (push kid rev-kids)))))


(defun add-kid-new (form kid)
    (let* ((rev-kids (reverse (gethash "kids" form))))
        (setf (gethash "kids" form) (reverse (push kid rev-kids)))))


(defun set-end (form pos)
    (setf (end form) pos))


(defun set-end-new (form pos)
    (when form
          (setf (gethash "end" form) pos)))


(defun set-end-offset (form pos)
    (setf (end-offset form) pos))


(defun set-end-offset-new (form pos)
    (when form
          (setf (gethash "endOffset" form) pos)))


(defun set-form-type (form value)
    (setf (form-type form) value))


(defun set-form-type-new (form value)
    (when form
          (setf (gethash "formType" form) value)))


(defun set-is-in-pkg (form value)
    (setf (in-pkg-p form) value))


(defun set-is-in-pkg-new (form value)
    (when form
          (setf (gethash "inPkg" form) value)))


(defun is-in-pkg (form)
    (when form
          (in-pkg-p form)))


(defun is-in-pkg-new (form)
    (when form
          (gethash "inPkg" form)))


(defun get-end (form)
    (when form
          (end form)))


(defun get-end-new (form)
    (when form
          (gethash "end" form)))


(defun get-end-offset (form)
    (when form
          (end-offset form)))


(defun get-end-offset-new (form)
    (when form
          (gethash "endOffset" form)))


(defun get-kids (form)
    (when form
          (kids form)))


(defun get-kids-new (form)
    (when form
          (gethash "kids" form)))


(defun get-start (form)
    (when form
          (start form)))


(defun get-start-new (form)
    (when form
          (gethash "start" form)))


(defun get-start-offset (form)
    (when form
          (start-offset form)))


(defun get-start-offset-new (form)
    (when form
          (gethash "startOffset" form)))


(defun get-form-type (form)
    (when form
          (form-type form)))


(defun get-form-type-new (form)
    (when form
          (gethash "formType" form)))


(defun create (&key start start-offset end end-offset form-type in-pkg kids)
    (make-instance 'form
        :start start
        :start-offset start-offset
        :end end
        :end-offset end-offset
        :form-type form-type
        :in-pkg-p in-pkg
        :kids kids))


(defun create-new (&key start start-offset end end-offset form-type in-pkg kids)
    (let ((form (make-hash-table :test #'equalp)))

        (setf (gethash "start" form) start)
        (setf (gethash "startOffset" form) start-offset)
        (setf (gethash "end" form) end)
        (setf (gethash "endOffset" form) end-offset)
        (setf (gethash "formType" form) form-type)
        (setf (gethash "inPkg" form) in-pkg)
        (setf (gethash "kids" form) kids)

        form))
