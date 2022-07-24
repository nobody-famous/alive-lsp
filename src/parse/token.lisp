(defpackage :alive/parse/token
    (:use :cl)
    (:export :clone
             :create
             :get-end
             :get-end-offset
             :get-type-value
             :get-text
             :get-start
             :get-start-offset
             :is-multiline
             :is-type)
    (:local-nicknames (:pos :alive/position)
                      (:types :alive/types)))

(in-package :alive/parse/token)


(defun get-type-value (obj)
    (gethash "typeValue" obj))


(defun get-text (obj)
    (gethash "text" obj))


(defun get-start (obj)
    (gethash "start" obj))


(defun get-start-offset (obj)
    (gethash "startOffset" obj))


(defun get-end (obj)
    (gethash "end" obj))


(defun get-end-offset (obj)
    (gethash "endOffset" obj))


(defun is-type (type token)
    (and token
         (= type (get-type-value token))))


(defun is-multiline (token)
    (and token
         (not (eq (pos:line (get-start token))
                  (pos:line (get-end token))))))


(defun create (&key type-value start start-offset end end-offset text)
    (let ((item (make-hash-table :test #'equalp)))

        (setf (gethash "typeValue" item) type-value)
        (setf (gethash "start" item) start)
        (setf (gethash "startOffset" item) start-offset)
        (setf (gethash "end" item) end)
        (setf (gethash "endOffset" item) end-offset)
        (setf (gethash "text" item) text)

        item))


(defun clone (obj new-start new-end &optional new-text)
    (loop :with copy := (make-hash-table :test #'equalp)

          :for value :being :the :hash-values :of obj
          :using (hash-key key)

          :do (cond ((equalp key "start") (setf (gethash key copy) new-start))
                    ((equalp key "end") (setf (gethash key copy) new-end))
                    ((equalp key "text") (setf (gethash key copy) new-text))
                    (T (setf (gethash key copy) value)))

          :finally (return copy)))