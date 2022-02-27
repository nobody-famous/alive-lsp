(require 'sb-introspect)

(defun foo (bar)
  (let* ((x 5)
         (y 10)
         (z (+ x y)))
    (format T "FOO CALLED ~A~%" z)))

(defparameter tmp (handler-case (compile-file "broken.lisp" :EMIT-CFASL nil)
                    (t (c)
                       (format T "Caught ~A~%" (type-of c))
                       c)))

(defun print-error (condition)
  (let ((context (sb-c::find-error-context nil)))
    (format T "HERE ~A~%~A~%~A~%"
            (sb-c::compiler-error-context-file-name context)
            (sb-c::compiler-error-context-original-source-path context)
            (sb-c::compiler-error-context-original-source context))))

(handler-bind ((sb-c:fatal-compiler-error #'print-error)
               (sb-c:compiler-error #'print-error))
  (compile-file "broken.lisp" :EMIT-CFASL nil))
