(defpackage :alive-lsp/test/compat/sbcl/compile
    (:use :cl)
    (:export :run-all)

    (:local-nicknames (:astreams :alive/streams))
)

(in-package :alive-lsp/test/compat/sbcl/compile)


(defun echo (msg)
    (format T "MESSAGE: ~A~%" msg)
)


; (defun compile-foo ()
;     (compile:file #'echo "test/compat/sbcl/files/foo.lisp")
; )


(defun create-ends (server)
    (let* ((port (usocket:get-local-port server))
           (writer (usocket:socket-connect "127.0.0.1" port :protocol :stream))
           (reader (usocket:socket-accept server))
          )
        (list reader writer)
    ))


(defun make-socket-pair ()
    (let* ((server (usocket:socket-listen "127.0.0.1" 0)))
        (unwind-protect
                (create-ends server)
            (usocket:socket-close server)
        )))


(defun print-output (stream out flags)
    (loop :until (first flags)
          :do (when (usocket::listen stream)
                    (format out "TRYING RECEIVE~%")
                    (format out "RECEIVED: ~A~%" (read stream))
              )))


; (defmethod sb-gray:stream-read-char ((str my-output-stream))
;     (let ((ch (elt (buffer str) (read-ndx str))))
;         (incf (read-ndx str))
;         ch
;     ))


; (defmethod sb-gray:stream-read-line ((str my-output-stream))
;     (let ((ch (elt (buffer str) (read-ndx str))))
;         (incf (read-ndx str))
;         (subseq (buffer str) 0 3)
;     ))


; (defmethod dump-stream ((str my-output-stream))
;     (format T "DUMP ~A~%" (buffer str))
; )



(defun read-stuff (stdout out-stream)
    (lambda ()
        (handler-case
                (loop :until (astreams:eof-p out-stream)
                      :do (let ((ch (read-char out-stream)))
                              (unless (eq ch nil)
                                      (format stdout "~A" ch)
                              )))
            (t (c) (format stdout "CONDITION ~A~%" c))
        )
        (format stdout "READ STUFF DONE~%")
    ))

(defun run-all ()
    (format T "SBCL Compile Tests~%")

    (let* ((orig-output *standard-output*)
           (out-stream (make-instance 'astreams:rt-stream))
           (err-stream (make-instance 'astreams:rt-stream))
           (*standard-output* out-stream)
           (*error-output* err-stream)
           (thread (bt:make-thread (read-stuff orig-output out-stream)))
          )
        ; (format orig-output "~A~%" (stream-element-type out-stream))
        ; (format out-stream "Testing...")
        ; (format orig-output "READ ~A~%" (read-char out-stream))
        ; (format orig-output "READ ~A~%" (read-char out-stream))
        ; (format orig-output "READ ~A~%" (read-char out-stream))
        ; (format orig-output "READ-LINE ~A~%" (read-line out-stream))

        (compile-file "test/compat/sbcl/files/foo.lisp")
        (close out-stream)
        (bt:join-thread thread)
        ; (dump-stream out-stream)
    )

    #+n (let ((s (make-string-output-stream)))
            (let* ((*standard-output* s)
                   (*error-output* s)
                  )
                (compile-foo)
            )

            (let ((tmp (make-string-input-stream (get-output-stream-string s))))

                (format T "STRING: ***~A***~%" (read-line tmp))
                (format T "STRING: ***~A***~%" (read-line tmp))
                (format T "STRING: ***~A***~%" (read-line tmp))
            ))

    #+n (destructuring-bind (reader writer)
                (make-socket-pair)
            (let* ((orig-stdout *standard-output*)
                   (*standard-output* (usocket:socket-stream writer))
                   (flags (cons nil nil))
                   (flush-thread (bt:make-thread (lambda ()
                                                     (loop :until (first flags)
                                                           :do (format orig-stdout "FLUSH OUTPUT~%")
                                                               (finish-output (usocket:socket-stream writer))
                                                               (sleep 0.5)
                                                     ))))
                   (thread (bt:make-thread (lambda ()
                                               (print-output (usocket:socket-stream reader) orig-stdout flags)
                                           ))))
                (compile-foo)
                (format orig-stdout "COMPILE DONE~%")

                (finish-output (usocket:socket-stream writer))
                (setf (first flags) t)

                (usocket:socket-close reader)
                (usocket:socket-close writer)
                (format orig-stdout "SOCKETS CLOSED~%")

                (format orig-stdout "WAITING FOR THREADS~%")

                (bt:join-thread thread)
                (format orig-stdout "PRINT THREAD DONE~%")

                ; (bt:join-thread flush-thread)
                (format orig-stdout "FLUSH THREAD DONE~%")
            )))


(defun foo ()
    (loop :for i :from 1 :to 3 :do
              (format T "~A~%" i)
              (sleep 1)
    ))
