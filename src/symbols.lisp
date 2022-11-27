(defpackage :alive/symbols
    (:use :cl)
    (:export :callable-p
             :get-all
             :get-all-names
             :get-lambda-list
             :get-location
             :get-source-file
             :external-p
             :function-p
             :lookup
             :macro-p)
    (:local-nicknames (:forms :alive/parse/forms)))

(in-package :alive/symbols)


(defun function-p (name &optional pkg-name)
    #+sbcl (alive/sbcl/symbols:function-p name pkg-name))


(defun get-lambda-list (fn-name &optional pkg-name)
    #+sbcl (alive/sbcl/symbols:get-lambda-list fn-name pkg-name))


(defun has-lambda-list-p (sym-name &optional pkg-name)
    (if (get-lambda-list sym-name pkg-name)
        T
        NIL))


(defun macro-p (sym-name &optional pkg-name)
    (let* ((pkg (if pkg-name
                    (find-package (string-upcase pkg-name))
                    *package*))
           (sym (when pkg
                      (find-symbol (string-upcase sym-name) pkg))))

        (if (macro-function sym)
            T
            NIL)))


(defun callable-p (sym-name &optional pkg-name)
    (or (function-p sym-name pkg-name)
        (macro-p sym-name pkg-name)
        (has-lambda-list-p sym-name pkg-name)))


(defun external-p (sym-name &optional pkg-name)
    (let* ((pkg (if pkg-name
                    (find-package (string-upcase pkg-name))
                    *package*)))

        (when pkg
              (multiple-value-bind (sym status)

                      (find-symbol (string-upcase sym-name) pkg)

                  (declare (ignore sym))

                  (or (eq status :external)
                      (eq status :inherited))))))


(defun lookup (name pkg-name)
    (let ((pkg (find-package (string-upcase pkg-name))))
        (when pkg
              (find-symbol (string-upcase name) pkg))))


(defun get-all (pkg)
    (let ((syms (list)))
        (do-symbols (s pkg syms)
            (push s syms))))


(defun get-all-names (pkg)
    (let ((syms (list)))
        (do-symbols (s pkg syms)
            (push (string-downcase (string s)) syms))))


(defun needs-encoding (char)
    (eq char #\:))


(defun encode-char (char)
    (if (needs-encoding char)
        (format nil "%~2,'0X" (char-code char))
        (string char)))


(defun url-encode (str)
    (let ((chars (map 'list (lambda (char)
                                (encode-char char))
                     str)))
        (apply #'concatenate 'string chars)))


(defun url-encode-filename (name)
    (let* ((raw-pieces (uiop:split-string name :separator "/\\"))
           (pieces (mapcar (lambda (piece)
                               (if (string= piece "")
                                   ""
                                   (format NIL "/~A" (url-encode piece))))
                           raw-pieces)))
        (apply #'concatenate 'string pieces)))


(defun lookup-sources (sym)
    (let ((types (list :class
                       :compiler-macro
                       :condition
                       :constant
                       :function
                       :generic-function
                       :macro
                       :method
                       :method-combination
                       :package
                       :setf-expander
                       :structure
                       :symbol-macro
                       :type
                       :alien-type
                       :variable
                       :declaration)))
        (reduce (lambda (out item)
                    (if out
                        out
                        (let* ((srcs (sb-introspect:find-definition-sources-by-name sym item))
                               (src (first srcs)))
                            (if (and src
                                     (sb-introspect:definition-source-pathname src))
                                src
                                nil))))
                types
            :initial-value nil)))


(defun get-range-from-file (file source-path)
    (handler-case
            (with-open-file (in-stream file)
                (let ((forms (forms:from-stream in-stream)))
                    (forms:get-range-for-path forms source-path)))
        (T nil)))


(defun get-source-file (sym)
    (let* ((src (when sym (lookup-sources sym)))
           (file (when src (sb-introspect:definition-source-pathname src))))

        (when file (namestring file))))


(defun get-location (sym)
    (let* ((src (when sym (lookup-sources sym)))
           (file (when src (sb-introspect:definition-source-pathname src)))
           (form-path (when src (sb-introspect:definition-source-form-path src))))

        (if file
            (list (url-encode-filename (namestring file))
                  (get-range-from-file file form-path))
            (list nil nil))))


(defun find-syms (pkg pref)
    (loop :with syms := ()
          :for sym :in (alive/symbols:get-all pkg)

          :do (let ((file (alive/symbols:get-source-file sym)))
                  (when (and file
                             (not (and (< 3 (length file))
                                       (string= "sys" (string-downcase file) :end1 3 :end2 3)))
                             (symbolp sym)
                             (alive/utils:fuzzy-match pref (symbol-name sym)))
                        (push (alive/symbols:get-location sym) syms)))

          :finally (return syms)))
