(defpackage :alive/session/state
    (:use :cl)
    (:export :add-history
             :add-inspector
             :add-listener
             :get-file-text
             :get-history-item
             :get-inspector
             :initialized
             :listener
             :listeners
             :lock
             :next-inspector-id
             :next-send-id
             :rem-inspector
             :set-file-text
             :set-initialized
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


(defmethod get-history-item ((obj state) index)
    (declare (integer index))
    (when (and (<= 0 index)
               (< index (length (history obj))))
          (elt (history obj) index)))


(defmethod add-listener ((obj state) (to-add listener))
    (push to-add (listeners obj)))


(defmethod set-initialized ((obj state) value)
    (declare (boolean value))
    (setf (initialized obj) value))


(defmethod set-file-text ((obj state) uri text)
    (declare (string uri text))
    (setf (gethash uri (files obj)) text))


(defmethod get-file-text ((obj state) uri)
    (declare (string uri))
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
    (declare (integer id) (alive/inspector:inspector inspector))
    (bt:with-recursive-lock-held ((lock obj))
        (setf (gethash id (inspectors obj))
            inspector)))


(defmethod rem-inspector ((obj state) &key id)
    (declare (integer id))
    (bt:with-recursive-lock-held ((lock obj))
        (remhash id (inspectors obj))))


(defmethod get-inspector ((obj state) &key id)
    (declare (integer id))
    (bt:with-recursive-lock-held ((lock obj))
        (gethash id (inspectors obj))))
