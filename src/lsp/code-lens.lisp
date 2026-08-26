(defpackage :alive/lsp/code-lens
    (:use :cl)
    (:export :xyz-get)
    (:shadow :get)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:types :alive/types)))

(in-package :alive/lsp/code-lens)


(defun has-code-lens (key)
    (or (string= key "defun")
        (string= key "defparameter")
        (string= key "defconstant")))


(defun get-text (text form)
    (string-downcase (subseq text
                             (form:get-start-offset form)
                             (form:get-end-offset form))))


(defun xyz-get-values (form)
    (let* ((keyword-form (first (form:get-kids form)))
           (name-form (second (form:get-kids form)))
           (pkg (form:get-package form)))
        (if (and keyword-form name-form)
            (values
                (form:xyz-get-end name-form)
                (form:xyz-get-sym-text keyword-form)
                (form:xyz-get-sym-text name-form)
                pkg)
            (values (alive/position:create 0 0) nil nil nil))))


(defun create-refs-lens (uri pos name pkg)
    (let ((lens (make-hash-table :test #'equalp))
          (cmd (make-hash-table :test #'equalp))
          (xrefs (ignore-errors (alive/sys/xref:find-references name pkg)))
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


(defun create-inspect-macro-lens (pos name pkg)
    (let ((lens (make-hash-table :test #'equalp))
          (cmd (make-hash-table :test #'equalp))
          (args (make-hash-table :test #'equalp)))

        (setf (gethash "title" cmd) "Inspect")
        (setf (gethash "command" cmd) "alive.queryInspectMacro")

        (setf (gethash "package" args) pkg)
        (setf (gethash "query" args) (format NIL "(~A)" name))
        (setf (gethash "arguments" cmd) (list args))

        (setf (gethash "range" lens) (alive/range:create pos pos))
        (setf (gethash "command" lens) cmd)

        lens))


(defun xyz-get (uri forms)
    (ignore-errors
        (loop :with lenses := nil

              :for form :in forms
              :do (multiple-value-bind (pos key name pkg)
                          (xyz-get-values form)
                      (if (string= key "defmacro")
                          (push (create-inspect-macro-lens pos name pkg) lenses)
                          (when (has-code-lens key)
                                (push (create-inspect-lens pos name pkg) lenses)
                                (push (create-refs-lens uri pos name pkg) lenses))))

              :finally (return lenses))))
