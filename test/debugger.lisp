(defpackage :alive/test/debugger
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:debugger :alive/debugger)))

(in-package :alive/test/debugger)

#+n ()

(defun bomb ()
    (when T
          (format NIL "Bomb called~%")
          (error "Bomb")))


(defun find-bomb-frame (frames)
    (loop :for frame :in frames
          :until (string-equal "bomb" (gethash "function" frame))
          :finally (return frame)))


(defun get-test-frame ()
    (handler-bind ((error (lambda (err)
                              (declare (ignore err))
                              (return-from get-test-frame
                                           (find-bomb-frame (alive/frames:list-debug-frames))))))
        (bomb)))


(defun test-forms ()
    (clue:test "Forms Test"
        (with-open-file (file "test/debugger.lisp")
            (alive/debugger:get-frame-loc
                file
                (get-test-frame)))))


(defun run-all ()
    (clue:suite "Debugger Tests"
        (test-forms)))
