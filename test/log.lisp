(defpackage :alive/test/log
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:logger :alive/logger)))

(in-package :alive/test/log)


(defun test-create ()
    (clue:test "Create default"
        (let ((logger (logger:create *standard-output*)))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*error*))
            (clue:check-equal :expected NIL
                              :actual (logger::has-level-new logger logger:*info*))
            (clue:check-equal :expected NIL
                              :actual (logger::has-level-new logger logger:*debug*))
            (clue:check-equal :expected NIL
                              :actual (logger::has-level-new logger logger:*trace*))))

    (clue:test "Create info"
        (let ((logger (logger:create *standard-output* logger:*info*)))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*error*))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*info*))
            (clue:check-equal :expected NIL
                              :actual (logger::has-level-new logger logger:*debug*))
            (clue:check-equal :expected NIL
                              :actual (logger::has-level-new logger logger:*trace*))))

    (clue:test "Create debug"
        (let ((logger (logger:create *standard-output* logger:*debug*)))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*error*))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*info*))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*debug*))
            (clue:check-equal :expected NIL
                              :actual (logger::has-level-new logger logger:*trace*))))

    (clue:test "Create trace"
        (let ((logger (logger:create *standard-output* logger:*trace*)))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*error*))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*info*))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*debug*))
            (clue:check-equal :expected T
                              :actual (logger::has-level-new logger logger:*trace*)))))


(defun run-all ()
    (clue:suite "Logger Tests"
        (test-create)))
