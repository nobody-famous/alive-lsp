(defpackage :alive/lsp/definition
    (:use :cl)
    (:export :get-location)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:packages :alive/packages)
                      (:state :alive/session/state)
                      (:sym :alive/symbols)
                      (:tokenizer :alive/parse/tokenizer)
                      (:utils :alive/utils)))

(in-package :alive/lsp/definition)


(defun get-sym-location (state sym)
    (let* ((src (when sym (sym:lookup-sources sym)))
           (file (when src (sb-introspect:definition-source-pathname src)))
           (forms (state:update-file-forms state file))
           (form-path (when src (sb-introspect:definition-source-form-path src))))

        (if file
            (list (utils:url-encode-filename (namestring (translate-logical-pathname file)))
                  (when forms (forms:get-range-for-path forms form-path)))
            (list nil nil))))


(defun get-location-for-name (state name pkg-name)
    (let ((sym (sym:lookup name pkg-name)))
        (get-sym-location state sym)))


(defun get-location (&key state forms pos)
    (let* ((pkg-name (packages:for-pos forms pos))
           (pkg (packages:lookup pkg-name))
           (*package* (or pkg *package*)))

        (multiple-value-bind (name pkg-name)
                (sym:for-pos forms pos)
            (when (and name pkg-name)
                  (get-location-for-name state name pkg-name)))))
