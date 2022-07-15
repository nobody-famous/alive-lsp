(defpackage :alive/inspector
    (:use :cl)
    (:export :create
             :get-text
             :get-pkg
             :get-result
             :to-result)
    (:local-nicknames (:eval :alive/eval)
                      (:astreams :alive/streams)))

(in-package :alive/inspector)


(defclass inspector ()
        ((text :accessor text
               :initform nil
               :initarg :text)
         (pkg :accessor pkg
              :initform nil
              :initarg :pkg)
         (result :accessor result
                 :initform nil
                 :initarg :result)))


(defun create (&key text pkg result)
    (make-instance 'inspector
        :text text
        :pkg pkg
        :result result))


(defun get-text (obj)
    (when obj
          (text obj)))


(defun get-pkg (obj)
    (when obj
          (pkg obj)))


(defun get-result (obj)
    (when obj
          (result obj)))


(defun fn-to-result (result sym name pkg-name)
    (setf (gethash "documentation" result)
        (documentation sym 'function))

    (setf (gethash "lambda-list" result)
        (princ-to-string (alive/symbols:get-lambda-list name pkg-name)))

    (setf (gethash "value" result)
        (with-output-to-string (str)
            (disassemble sym :stream str))))


(defun hash-table-to-result (result obj)
    (setf (gethash "count" result)
        (hash-table-count obj))

    (setf (gethash "test" result)
        (hash-table-test obj))

    (setf (gethash "rehash-size" result)
        (hash-table-rehash-size obj))

    (setf (gethash "size" result)
        (hash-table-size obj))

    (let ((value-map (make-hash-table)))
        (loop :for value :being :the :hash-values :of obj :using (hash-key key)
              :do (setf (gethash key value-map)
                      (princ-to-string value)))

        (setf (gethash "value" result)
            value-map)))


(defun list-to-result (result obj)
    (setf (gethash "length" result)
        (length obj))

    (setf (gethash "value" result)
        (mapcar #'princ-to-string obj)))


(defun vector-to-result (result obj)
    (setf (gethash "length" result)
        (length obj))

    (setf (gethash "value" result)
        (map 'vector #'princ-to-string obj)))


(defun sym-to-result (result obj)
    (let* ((name (string obj))
           (pkg (symbol-package obj))
           (pkg-name (package-name pkg)))

        (cond ((alive/symbols:function-p name pkg-name)
                  (fn-to-result result obj name pkg-name))

              ((alive/symbols:macro-p name pkg-name)
                  (fn-to-result result obj name pkg-name))

              (T (setf (gethash "value" result)
                     (princ-to-string obj))))))


(defun to-result (obj)
    (let ((result (make-hash-table :test #'equalp))
          (obj (if (symbolp obj)
                   (symbol-value obj)
                   obj)))

        (typecase obj
            (symbol (sym-to-result result obj))
            (hash-table (hash-table-to-result result obj))
            (cons (list-to-result result obj))
            (vector (vector-to-result result obj))
            (otherwise (setf (gethash "value" result)
                           (princ-to-string obj))))

        result))
