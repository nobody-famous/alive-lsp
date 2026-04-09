(defpackage :alive/lsp/code-lens
    (:use :cl)
    (:export :get-unresolved)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)))

(in-package :alive/lsp/code-lens)


(declaim (ftype (function ((or null string)) (or cons null)) get-unresolved))
(defun get-unresolved (text)
    (loop :with forms := (forms:from-stream-or-nil (make-string-input-stream text))
          :for form :in forms
          :do (when (hash-table-p (first (gethash "kids" form)))
                    (format T "GET UNRESOLVED ~A~%" (subseq text
                                                            (form:get-start-offset (first (gethash "kids" form)))
                                                            (form:get-end-offset (first (gethash "kids" form)))))))
    nil)

#+n (let ((item (make-hash-table :test #'equalp))
          (cmd (make-hash-table :test #'equalp))
          (data (make-hash-table :test #'equalp)))
        ; (setf (gethash "title" cmd) "0 references")
        ; (setf (gethash "command" cmd) "editor.action.showReferences")
        ; (setf (gethash "arguments" cmd) (list uri
        ;                                       (alive/position:create 36 7)))
        (setf (gethash "range" item) (alive/range:create (alive/position:create 36 0) (alive/position:create 36 0)))

        (setf (gethash "uri" data) uri)
        (setf (gethash "line" data) 36)
        (setf (gethash "character" data) 0)

        (setf (gethash "data" item) data)
        ; (setf (gethash "command" item) cmd)

        (push item items))