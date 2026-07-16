(defpackage :alive/parse/token
    (:use :cl)
    (:export :clone
             :create
             :xyz-create
             :get-end
             :xyz-get-end
             :get-end-offset
             :xyz-get-end-offset
             :get-type-value
             :xyz-get-type-value
             :get-text
             :get-start
             :xyz-get-start
             :get-start-offset
             :xyz-get-start-offset
             :is-multiline
             :is-type
             :xyz-is-type)
    (:local-nicknames (:pos :alive/position)
                      (:types :alive/types)))

(in-package :alive/parse/token)


(defstruct token
    type-value
    text
    start
    start-offset
    end
    end-offset)


(defun get-type-value (obj)
    (when obj
          (gethash "typeValue" obj)))


(defun xyz-get-type-value (token)
    (when (token-p token)
          (token-type-value token)))


(defun get-text (obj)
    (when obj
          (gethash "text" obj)))


(defun get-start (obj)
    (when obj
          (gethash "start" obj)))


(defun xyz-get-start (token)
    (when (token-p token)
          (token-start token)))


(defun get-start-offset (obj)
    (when obj
          (gethash "startOffset" obj)))


(defun xyz-get-start-offset (token)
    (when (token-p token)
          (token-start-offset token)))


(defun get-end (obj)
    (when obj
          (gethash "end" obj)))


(defun xyz-get-end (token)
    (when (token-p token)
          (token-end token)))


(defun get-end-offset (obj)
    (when obj
          (gethash "endOffset" obj)))


(defun xyz-get-end-offset (token)
    (when (token-p token)
          (token-end-offset token)))


(defun is-type (type token)
    (and token
         (= type (get-type-value token))))


(defun xyz-is-type (type token)
    (and (token-p token)
         (= type (xyz-get-type-value token))))


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


(defun xyz-create (&key type-value start start-offset end end-offset text)
    (make-token :type-value type-value
                :start start
                :start-offset start-offset
                :end end
                :end-offset end-offset
                :text text))


(defun clone (obj new-start new-end &optional new-text)
    (loop :with copy := (make-hash-table :test #'equalp)

          :for value :being :the :hash-values :of obj
          :using (hash-key key)

          :do (cond ((equalp key "start") (setf (gethash key copy) new-start))
                    ((equalp key "end") (setf (gethash key copy) new-end))
                    ((equalp key "text") (setf (gethash key copy) new-text))
                    (T (setf (gethash key copy) value)))

          :finally (return copy)))
