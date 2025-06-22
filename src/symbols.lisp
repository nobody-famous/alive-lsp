(defpackage :alive/symbols
    (:use :cl)
    (:export :callable-p
             :get-all
             :get-all-names
             :get-lambda-list
             :get-location
             :get-source-file
             :external-p
             :find-tokens
             :for-pos
             :function-p
             :lookup
             :macro-p)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:packages :alive/packages)
                      (:pos :alive/position)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)
                      (:utils :alive/utils)))

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
            (list (utils:url-encode-filename (namestring file))
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


(defun find-tokens (tokens pos)
    (loop :for token :in tokens
          :collect token :into found-tokens
          :while (pos:less-than (token:get-end token) pos)
          :finally (return (cond ((<= 3 (length found-tokens)) (subseq (reverse found-tokens) 0 3))
                                 ((= 2 (length found-tokens)) (reverse (cons nil found-tokens)))
                                 ((= 1 (length found-tokens)) (list (first found-tokens) nil nil))))))


(defun for-pos (text pos)
    (let* ((raw-tokens (tokenizer:from-stream (make-string-input-stream text)))
           (tokens (find-tokens raw-tokens pos))
           (pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (or pkg *package*)))

        (unless (zerop (length tokens))
            (destructuring-bind (token1 token2 token3) tokens
                (cond ((and (eq (token:get-type-value token1) types:*symbol*)
                            (eq (token:get-type-value token2) types:*colons*)
                            (eq (token:get-type-value token3) types:*symbol*))
                          (let* ((real-pkg (packages:lookup (token:get-text token3)))
                                 (real-pkg-name (if real-pkg
                                                    (package-name real-pkg)
                                                    (token:get-text token3))))
                              (values (token:get-text token1)
                                  real-pkg-name)))

                      ((eq (token:get-type-value token1) types:*symbol*)
                          (values (token:get-text token1)
                              pkg-name))

                      (T nil))))))
