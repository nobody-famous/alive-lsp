(defpackage :alive/sys/xref
    (:use :cl)
    (:export :xyz-find-references
             :xyz-get-locations)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:loc :alive/location)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:sym :alive/symbols)
                      (:utils :alive/utils)))

(in-package :alive/sys/xref)


(defun find-callers (name pkg-name)
    (let ((to-find (sym:lookup name pkg-name)))
        (cond ((sym:function-p name pkg-name) (sb-introspect:who-calls to-find))
              ((sym:macro-p name pkg-name) (sb-introspect:who-macroexpands to-find))
              (T (sb-introspect:who-references to-find)))))


(defun caller-to-location (caller)
    (let* ((path (sb-introspect:definition-source-pathname (cdr caller)))
           (path-str (if path
                         (namestring (sb-introspect:definition-source-pathname (cdr caller)))
                         nil)))
        (list (cons :file path-str)
              (cons :form-path (sb-introspect:definition-source-form-path (cdr caller))))))


(defun lookup-references (name pkg-name)
    (let* ((locations (mapcar #'caller-to-location (find-callers name pkg-name))))
        (remove-if-not (lambda (caller)
                           (stringp (cdr (assoc :file caller))))
                locations)))


(defun xyz-read-file-forms (file)
    (with-open-file (s file)
        (alive/parse/forms:xyz-from-stream s)))


(defun xyz-get-file-forms (refs)
    (loop :with file := nil
          :with forms := (make-hash-table :test #'equalp)
          :for ref :in refs
          :do (setf file (cdr (assoc :file ref)))
              (unless (gethash file forms)
                  (setf (gethash file forms) (xyz-read-file-forms file)))
          :finally (return forms)))


(defun xyz-ref-to-location (file-forms ref)
    (let* ((file (cdr (assoc :file ref)))
           (form-path (cdr (assoc :form-path ref)))
           (forms (gethash file file-forms))
           (range (forms:xyz-get-range-for-path forms form-path)))
        (loc:create (utils:url-encode-filename file)
                    range)))


(defun xyz-find-references (name pkg-name)
    (let* ((refs (lookup-references name pkg-name))
           (file-forms (xyz-get-file-forms refs)))
        (mapcar (lambda (ref)
                    (xyz-ref-to-location file-forms ref))
                refs)))


(defun xyz-get-locations (text pos)
    (multiple-value-bind (name pkg-name)
            (sym:for-pos text pos)
        (when (and name pkg-name)
              (xyz-find-references name pkg-name))))
