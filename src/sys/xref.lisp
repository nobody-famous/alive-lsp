(defpackage :alive/sys/xref
    (:use :cl)
    (:export :get-locations)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:loc :alive/location)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:sym :alive/symbols)
                      (:utils :alive/utils)))

(in-package :alive/sys/xref)


(declaim (ftype (function (string string) *) find-callers))
(defun find-callers (name pkg-name)
    (let ((to-find (sym:lookup name pkg-name)))
        (cond ((sym:function-p name pkg-name) (sb-introspect:who-calls to-find))
              ((sym:macro-p name pkg-name) (sb-introspect:who-macroexpands to-find))
              (T (sb-introspect:who-references to-find)))))


(declaim (ftype (function (cons) (or null cons)) caller-to-location))
(defun caller-to-location (caller)
    (let* ((path (sb-introspect:definition-source-pathname (cdr caller)))
           (path-str (if path
                         (namestring (sb-introspect:definition-source-pathname (cdr caller)))
                         nil)))
        (list (cons :file path-str)
              (cons :form-path (sb-introspect:definition-source-form-path (cdr caller))))))


(declaim (ftype (function (string string) (or null cons)) lookup-references))
(defun lookup-references (name pkg-name)
    (let* ((locations (mapcar #'caller-to-location (find-callers name pkg-name))))
        (remove-if-not (lambda (caller)
                           (stringp (cdr (assoc :file caller))))
                locations)))


(declaim (ftype (function (string) (values list &optional)) read-file-forms))
(defun read-file-forms (file)
    (with-open-file (s file)
        (alive/parse/forms:from-stream s)))


(declaim (ftype (function ((or null cons)) hash-table) get-file-forms))
(defun get-file-forms (refs)
    (loop :with file := nil
          :with forms := (make-hash-table :test #'equalp)
          :for ref :in refs
          :do (setf file (cdr (assoc :file ref)))
              (unless (gethash file forms)
                  (setf (gethash file forms) (read-file-forms file)))
          :finally (return forms)))


(declaim (ftype (function (hash-table cons) (values loc:text-location &optional)) ref-to-location))
(defun ref-to-location (file-forms ref)
    (let* ((file (cdr (assoc :file ref)))
           (forms (gethash file file-forms))
           (form-path (cdr (assoc :form-path ref)))
           (form (forms:get-nth-form forms (first form-path))))
        (loc:create (utils:url-encode-filename file)
                    (range:create (form:get-start form)
                                  (form:get-end form)))))


(declaim (ftype (function (string string) (or null cons)) find-references))
(defun find-references (name pkg-name)
    (let* ((refs (lookup-references name pkg-name))
           (file-forms (get-file-forms refs)))
        (mapcar (lambda (ref)
                    (ref-to-location file-forms ref))
                refs)))


(declaim (ftype (function (string pos:text-position) (or null cons)) get-locations))
(defun get-locations (text pos)
    (multiple-value-bind (name pkg-name)
            (sym:for-pos text pos)
        (when (and name pkg-name)
              (find-references name pkg-name))))
