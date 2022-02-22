(defpackage :alive/lsp/message/alive/load-file
    (:use :cl)
    (:export :create-params
             :create-request
             :from-wire)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:types :alive/types)))

(in-package :alive/lsp/message/alive/load-file)


(defclass load-file (message:request)
    ((message::method :initform "$/alive/loadFile")))


(defmethod print-object ((obj load-file) out)
    (format out "{method: ~A; params: ~A}"
            (message:method-name obj)
            (message:params obj)))


(defmethod types:deep-equal-p ((a load-file) b)
    (and (equal (type-of a) (type-of b))
         (types:deep-equal-p (message:params a) (message:params b))))


(defclass params ()
    ((path :accessor path
           :initform nil
           :initarg :path)))


(defmethod print-object ((obj params) out)
    (format out "{path: ~A}"
            (path obj)))


(defmethod types:deep-equal-p ((a params) b)
    (and (equal (type-of a) (type-of b))
         (string= (path a) (path b))))


(defun create-request (params)
    (make-instance 'load-file :params params))


(defun create-params (&key path)
    (make-instance 'params :path path))


(defun from-wire (params)
    (labels ((add-param (params key value)
                  (cond ((eq key :path) (setf (path params) value)))))

        (loop :with out-params := (make-instance 'params)
              :for param :in params :do
                  (add-param out-params (car param) (cdr param))
              :finally (return (create-request out-params)))))
