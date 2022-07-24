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


(defmethod get-type-value ((obj hash-table))
    (gethash "typeValue" obj))


(defmethod get-text ((obj hash-table))
    (gethash "text" obj))


(defmethod get-start ((obj hash-table))
    (gethash "start" obj))


(defmethod get-start-offset ((obj hash-table))
    (gethash "startOffset" obj))


(defmethod get-end ((obj hash-table))
    (gethash "end" obj))


(defmethod get-end-offset ((obj hash-table))
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


(defmethod clone ((obj hash-table) new-start new-end &optional new-text)
    (create :type-value (get-type-value obj)
            :text (if new-text
                      new-text
                      (get-text obj))
            :start new-start
            :end new-end))