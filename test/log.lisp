(defpackage :alive/test/log
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:logger :alive/logger)))

(in-package :alive/test/log)


(defun test-create ()
    (clue:suite "Create"
        (clue:test "Create default"
            (logger:with-logging (logger:create *standard-output*)
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*error*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level logger:*info*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level logger:*debug*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level logger:*trace*))))

        (clue:test "Create info"
            (logger:with-logging (logger:create *standard-output* logger:*info*)
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*error*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*info*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level logger:*debug*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level logger:*trace*))))

        (clue:test "Create debug"
            (logger:with-logging (logger:create *standard-output* logger:*debug*)
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*error*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*info*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*debug*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level logger:*trace*))))

        (clue:test "Create trace"
            (logger:with-logging (logger:create *standard-output* logger:*trace*)
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*error*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*info*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*debug*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level logger:*trace*))))))


(defun write-log (level fn)
    (with-output-to-string (out)
        (let ((logger (logger:create out level)))
            (logger:with-logging logger
                (funcall fn)))))


(defun test-write ()
    (clue:suite "Write"
        (clue:test "Write error"
            (let ((actual (write-log logger:*error*
                                     (lambda ()
                                         (logger:error-msg "Checking ~A" 5)))))
                (clue:check-exists (search "Checking 5" actual))))

        (clue:test "Write info"
            (let ((actual (write-log logger:*info*
                                     (lambda ()
                                         (logger:info-msg "Checking ~A" 5)))))
                (clue:check-exists (search "Checking 5" actual))))

        (clue:test "Write debug"
            (let ((actual (write-log logger:*debug*
                                     (lambda ()
                                         (logger:debug-msg "Checking ~A" 5)))))
                (clue:check-exists (search "Checking 5" actual))))

        (clue:test "Write trace"
            (let ((actual (write-log logger:*trace*
                                     (lambda ()
                                         (logger:trace-msg "Checking ~A" 5)))))
                (clue:check-exists (search "Checking 5" actual))))))


(defun test-write-fail ()
    (clue:suite "Write fail"
        (clue:test "Write error fail"
            (let ((actual (write-log logger:*error*
                                     (lambda ()
                                         (logger:info-msg "Checking ~A" 5)))))
                (clue:check-equal :expected nil
                                  :actual (search "Checking 5" actual))))

        (clue:test "Write info fail"
            (let ((actual (write-log logger:*info*
                                     (lambda ()
                                         (logger:debug-msg "Checking ~A" 5)))))
                (clue:check-equal :expected nil
                                  :actual (search "Checking 5" actual))))

        (clue:test "Write info fail"
            (let ((actual (write-log logger:*debug*
                                     (lambda ()
                                         (logger:trace-msg "Checking ~A" 5)))))
                (clue:check-equal :expected nil
                                  :actual (search "Checking 5" actual))))))


(defun run-all ()
    (clue:suite "Logger Tests"
        (test-create)
        (test-write)
        (test-write-fail)))
