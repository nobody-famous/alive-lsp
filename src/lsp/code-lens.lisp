(defpackage :alive/lsp/code-lens
    (:use :cl)
    (:export :get)
    (:shadow :get)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:types :alive/types)))

(in-package :alive/lsp/code-lens)


(declaim (ftype (function ((or null string)) T) has-code-lens))
(defun has-code-lens (key)
    (or (string= key "defun")
        (string= key "defparameter")
        (string= key "defconstant")))


(declaim (ftype (function (string hash-table) string) get-text))
(defun get-text (text form)
    (string-downcase (subseq text
                             (form:get-start-offset form)
                             (form:get-end-offset form))))


(declaim (ftype (function (string hash-table) (values alive/position:text-position (or null string) (or null string) (or null string))) get-values))
(defun get-values (text form)
    (let* ((keyword-form (first (gethash "kids" form)))
           (name-form (second (gethash "kids" form))))
        (if (and keyword-form name-form)
            (values
                (form:get-end name-form)
                (get-text text keyword-form)
                (get-text text name-form)
                (alive/packages:for-pos text (form:get-start keyword-form)))
            (values (alive/position:create 0 0) nil nil nil))))


(defun create-refs-lens (uri pos name pkg)
    (let ((lens (make-hash-table :test #'equalp))
          (cmd (make-hash-table :test #'equalp))
          (xrefs (alive/sys/xref:find-references name pkg))
          (path (if (alive/utils:has-prefix uri "file://")
                    uri
                    (format nil "file://~A" uri))))

        (setf (gethash "title" cmd) (format nil "~A references" (length xrefs)))
        (setf (gethash "command" cmd) "alive.showReferences")
        (setf (gethash "arguments" cmd) (list path pos xrefs))

        (setf (gethash "range" lens) (alive/range:create pos pos))
        (setf (gethash "command" lens) cmd)

        lens))


(defun create-inspect-lens (pos name pkg)
    (let ((lens (make-hash-table :test #'equalp))
          (cmd (make-hash-table :test #'equalp))
          (args (make-hash-table :test #'equalp)))

        (setf (gethash "title" cmd) "Inspect")
        (setf (gethash "command" cmd) "alive.inspect")

        (setf (gethash "name" args) name)
        (setf (gethash "package" args) pkg)
        (setf (gethash "arguments" cmd) (list args))

        (setf (gethash "range" lens) (alive/range:create pos pos))
        (setf (gethash "command" lens) cmd)

        lens))


(declaim (ftype (function (string (or null string)) (or cons null)) get))
(defun get (uri text)
    (loop :with forms := (forms:from-stream-or-nil (make-string-input-stream text))
          :with lenses := nil

          :for form :in forms
          :do (multiple-value-bind (pos key name pkg)
                      (get-values text form)
                  (when (has-code-lens key)
                        (push (create-inspect-lens pos name pkg) lenses)
                        (push (create-refs-lens uri pos name pkg) lenses)))

          :finally (return lenses)))
