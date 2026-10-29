(defpackage :alive/sys/xref
    (:use :cl)
    (:export :find-references
             :get-locations)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:loc :alive/location)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:state :alive/session/state)
                      (:sym :alive/symbols)
                      (:tokenizer :alive/parse/tokenizer)
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


(defun read-file-forms (file)
    (with-open-file (s file)
        (let ((tokens (tokenizer:xyz-from-stream s)))
            (alive/parse/forms:from-tokens tokens))))


(defun get-file-forms (state refs)
    (loop :with file := nil
          :with forms := (make-hash-table :test #'equalp)
          :for ref :in refs
          :do (let* ((file (cdr (assoc :file ref)))
                     (uri (utils:url-encode-filename file))
                     (uri-forms (state:get-file-forms state uri)))
                  (unless (gethash file forms)
                      (unless uri-forms
                          (setf uri-forms (read-file-forms file))
                          (state:set-file-forms state uri uri-forms))
                      (when uri-forms
                            (setf (gethash file forms) uri-forms))))
          :finally (return forms)))


(defun ref-to-location (file-forms ref)
    (let* ((file (cdr (assoc :file ref)))
           (form-path (cdr (assoc :form-path ref)))
           (forms (gethash file file-forms))
           (range (forms:get-range-for-path forms form-path)))
        (loc:create (utils:url-encode-filename file)
                    range)))


(defun find-references (state name pkg-name)
    (let* ((refs (lookup-references name pkg-name))
           (file-forms (get-file-forms state refs)))
        (mapcar (lambda (ref)
                    (ref-to-location file-forms ref))
                refs)))


(defun get-locations (state forms pos)
    (multiple-value-bind (name pkg-name)
            (sym:for-pos forms pos)
        (when (and name pkg-name)
              (find-references state name pkg-name))))
