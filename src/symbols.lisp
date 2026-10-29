(defpackage :alive/symbols
    (:use :cl)
    (:export :add-bars
             :callable-p
             :external-p
             :for-pos
             :function-p
             :get-all-names
             :get-lambda-list
             :get-location
             :lookup
             :lookup-sources
             :macro-p
             :special-ch-p)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:packages :alive/packages)
                      (:pos :alive/position)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)
                      (:utils :alive/utils)))

(in-package :alive/symbols)


(defun special-ch-p (ch)
    (or (and (alpha-char-p ch)
             (lower-case-p ch))
        (char= #\" ch)
        (char= #\' ch)
        (char= #\( ch)
        (char= #\) ch)
        (char= #\, ch)
        (char= #\: ch)
        (char= #\; ch)
        (char= #\` ch)))


(defun add-bars (name)
    (if (some (lambda (ch)
                  (special-ch-p ch)) name)
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


(defun for-pos (forms pos)
    (let* ((top-form (forms:get-top-form forms pos))
           (expr (forms:find-expr top-form pos))
           (pkg-name (packages:for-pos forms pos)))

        (when expr
              (packages:for-tokens (form:get-tokens expr) pkg-name))))
