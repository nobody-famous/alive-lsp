(defpackage :alive/test/log
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:logger :alive/logger)))

(in-package :alive/test/log)


(defun test-create ()
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
                              :actual (logger:has-level logger:*trace*)))))


(defun write-log (fn level fmt args)
    (with-output-to-string (out)
        (let ((logger (logger:create out level)))
            (logger:with-logging logger
                (funcall fn fmt args)))))


(defun test-write ()
    #+n (clue:test "Write error"
            (let ((actual (write-log #'logger:error-msg logger:*error* "Checking ~A" 5)))
                (clue:check-exists (search "Checking 5" actual))))

    #+n (clue:test "Write info"
            (let ((actual (write-log #'logger:info-msg logger:*info* "Checking ~A" 5)))
                (clue:check-exists (search "Checking 5" actual))))

    #+n (clue:test "Write debug"
            (let ((actual (write-log #'logger:debug-msg logger:*debug* "Checking ~A" 5)))
                (clue:check-exists (search "Checking 5" actual))))

    #+n (clue:test "Write trace"
            (let ((actual (write-log #'logger:trace-msg logger:*trace* "Checking ~A" 5)))
                (clue:check-exists (search "Checking 5" actual)))))


(defun test-write-fail ()
    #+n (clue:test "Write error fail"
            (let ((actual (write-log #'logger:info-msg logger:*error* "Checking ~A" 5)))
                (clue:check-equal :expected nil
                                  :actual (search "Checking 5" actual))))

    #+n (clue:test "Write info fail"
            (let ((actual (write-log #'logger:debug-msg logger:*info* "Checking ~A" 5)))
                (clue:check-equal :expected nil
                                  :actual (search "Checking 5" actual))))

    #+n (clue:test "Write debug fail"
            (let ((actual (write-log #'logger:trace-msg logger:*debug* "Checking ~A" 5)))
                (clue:check-equal :expected nil
                                  :actual (search "Checking 5" actual)))))


(defun run-all ()
    (clue:suite "Logger Tests"
        (test-create)
        (test-write)
        (test-write-fail)))
