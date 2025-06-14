(defpackage :alive/test/log
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:logger :alive/logger)))

(in-package :alive/test/log)


(defun test-create ()
    (clue:suite "Create"
        (clue:test "Create default"
            (let ((log (logger:create *standard-output*)))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*error*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level log logger:*info*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level log logger:*debug*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level log logger:*trace*))))

        (clue:test "Create info"
            (let ((log (logger:create *standard-output* logger:*info*)))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*error*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*info*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level log logger:*debug*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level log logger:*trace*))))

        (clue:test "Create debug"
            (let ((log (logger:create *standard-output* logger:*debug*)))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*error*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*info*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*debug*))
                (clue:check-equal :expected NIL
                                  :actual (logger:has-level log logger:*trace*))))

        (clue:test "Create trace"
            (let ((log (logger:create *standard-output* logger:*trace*)))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*error*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*info*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*debug*))
                (clue:check-equal :expected T
                                  :actual (logger:has-level log logger:*trace*))))))


(defun write-log (level fn)
    (with-output-to-string (out)
        (let ((log (logger:create out level)))
            (funcall fn log))))


(defun test-write ()
    (clue:suite "Write"
        (clue:test "Write error"
            (let ((actual (write-log logger:*error*
                                     (lambda (log)
                                         (logger:error-msg log "Checking ~A" 5)))))
                (clue:check-exists (search "Checking 5" actual))))

        (clue:test "Write info"
            (let ((actual (write-log logger:*info*
                                     (lambda (log)
                                         (logger:info-msg log "Checking ~A" 5)))))
                (clue:check-exists (search "Checking 5" actual))))

        (clue:test "Write debug"
            (let ((actual (write-log logger:*debug*
                                     (lambda (log)
                                         (logger:debug-msg log "Checking ~A" 5)))))
                (clue:check-exists (search "Checking 5" actual))))

        (clue:test "Write trace"
            (let ((actual (write-log logger:*trace*
                                     (lambda (log)
                                         (logger:trace-msg log "Checking ~A" 5)))))
                (clue:check-exists (search "Checking 5" actual))))))


(defun test-write-fail ()
    (clue:suite "Write fail"
        (clue:test "Write error fail"
            (let ((actual (write-log logger:*error*
                                     (lambda (log)
                                         (logger:info-msg log "Checking ~A" 5)))))
                (clue:check-equal :expected nil
                                  :actual (search "Checking 5" actual))))

        (clue:test "Write info fail"
            (let ((actual (write-log logger:*info*
                                     (lambda (log)
                                         (logger:debug-msg log "Checking ~A" 5)))))
                (clue:check-equal :expected nil
                                  :actual (search "Checking 5" actual))))

        (clue:test "Write info fail"
            (let ((actual (write-log logger:*debug*
                                     (lambda (log)
                                         (logger:trace-msg log "Checking ~A" 5)))))
                (clue:check-equal :expected nil
                                  :actual (search "Checking 5" actual))))))


(defun run-all ()
    (clue:suite "Logger Tests"
        (test-create)
        (test-write)
        (test-write-fail)))
