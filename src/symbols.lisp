(defpackage :alive/symbols
    (:use :cl)
    (:export :callable-p
             :escape
             :external-p
             :find-tokens
             :for-pos
             :function-p
             :get-all-names
             :get-lambda-list
             :get-location
             :get-source-file
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


(defun escape (name)
    (if (some (lambda (ch)
                  (and (alpha-char-p ch)
                       (lower-case-p ch))) name)
        (format nil "|~A|" name)
        (string-downcase name)))


(defun function-p (name &optional pkg-name)
    #+sbcl (alive/sbcl/symbols:function-p name pkg-name))


(defun get-lambda-list (fn-name &optional pkg-name)
    #+sbcl (alive/sbcl/symbols:get-lambda-list fn-name pkg-name))


(defun has-lambda-list-p (sym-name &optional pkg-name)
    (if (get-lambda-list sym-name pkg-name)
        T
        NIL))


(defun macro-p (sym-name &optional pkg-name)
    (let ((sym (utils:lookup-symbol sym-name pkg-name)))
        (if (macro-function sym)
            T
            NIL)))


(defun callable-p (sym-name &optional pkg-name)
    (or (function-p sym-name pkg-name)
        (macro-p sym-name pkg-name)
        (has-lambda-list-p sym-name pkg-name)))


(defun external-p (sym-name &optional pkg-name)
    (multiple-value-bind (sym status)

            (utils:lookup-symbol sym-name pkg-name)

        (declare (ignore sym))

        (or (eq status :external)
            (eq status :inherited))))


(defun lookup (name pkg-name)
    (utils:lookup-symbol name pkg-name))


(defun get-all-names (pkg)
    (let ((syms (list)))
        (do-symbols (s pkg syms)
            (push (string s) syms))))


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
