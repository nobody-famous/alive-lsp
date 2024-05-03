(defpackage :alive/session/state
    (:use :cl)
    (:export :lock
             :state))

(in-package :alive/session/state)


(defclass listener ()
        ((on-done :accessor on-done
                  :initform nil
                  :initarg :on-done)))


(defclass state ()
        ((running :accessor running
                  :initform T
                  :initarg :running)
         (initialized :accessor initialized
                      :initform nil
                      :initarg :initialized)
         (files :accessor files
                :initform (make-hash-table :test 'equalp)
                :initarg :files)
         (thread-msgs :accessor thread-msgs
                      :initform (make-hash-table :test 'equalp)
                      :initarg :thread-msgs)
         (listeners :accessor listeners
                    :initform nil
                    :initarg :listeners)
         (thread-name-id :accessor thread-name-id
                         :initform 1
                         :initarg :thread-name-id)
         (lock :accessor lock
               :initform (bt:make-recursive-lock)
               :initarg :lock)
         (send-msg-id :accessor send-msg-id
                      :initform 1
                      :initarg :send-msg-id)
         (sent-msg-callbacks :accessor sent-msg-callbacks
                             :initform (make-hash-table :test 'equalp)
                             :initarg :sent-msg-callbacks)
         (inspector-id :accessor inspector-id
                       :initform 1
                       :initarg :inspector-id)
         (inspectors :accessor inspectors
                     :initform (make-hash-table :test 'equalp)
                     :initarg :inspectors)
         (input-cond-vars :accessor input-cond-vars
                          :initform (make-hash-table :test 'equalp)
                          :initarg :input-cond-vars)
         (history :accessor history
                  :initform (make-array 3)
                  :initarg :history)
         (read-thread :accessor read-thread
                      :initform nil
                      :initarg :read-thread)))


(defmethod add-history ((obj state) item)
    (setf (elt (history obj) 2)
        (elt (history obj) 1))
    (setf (elt (history obj) 1)
        (elt (history obj) 0))
    (setf (elt (history obj) 0)
        item))


(defmethod add-listener ((obj state) (to-add listener))
    (push to-add (listeners obj)))


(defmethod set-initialized ((obj state) value)
    (setf (initialized obj) value))


(defmethod set-file-text ((obj state) uri text)
    (setf (gethash uri (files obj)) text))


(defmethod get-file-text ((obj state) uri)
    (gethash uri (files obj)))


(defmethod next-send-id ((obj state))
    (bt:with-recursive-lock-held ((lock obj))
        (let ((id (send-msg-id obj)))
            (incf (send-msg-id obj))
            id)))


(defmethod next-inspector-id ((obj state))
    (bt:with-recursive-lock-held ((lock obj))
        (let ((id (inspector-id obj)))
            (incf (inspector-id obj))
            id)))


(defmethod add-inspector ((obj state) &key id inspector)
    (bt:with-recursive-lock-held ((lock obj))
        (setf (gethash id (inspectors obj))
            inspector)))


(defmethod rem-inspector ((obj state) &key id)
    (bt:with-recursive-lock-held ((lock obj))
        (remhash id (inspectors obj))))


(defmethod get-inspector ((obj state) &key id)
    (bt:with-recursive-lock-held ((lock obj))
        (gethash id (inspectors obj))))
