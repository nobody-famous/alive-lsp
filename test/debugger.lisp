(defpackage :alive/test/debugger
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:debugger :alive/debugger)
                      (:pos :alive/position)))

(in-package :alive/test/debugger)

#+n ()

(defun bomb ()
    (when T
          (format NIL "Bomb called~%")
          (error "Bomb")))


(defun bomb-2 ()
    (when T
          (format NIL "Bomb called~%")
          (format NIL "Bomb called~%"))
    (when T
          (error "Bomb")))


(defun find-bomb-frame (frames name)
    (loop :for frame :in frames
          :until (string-equal name (gethash "function" frame))
          :finally (return frame)))


(defun get-test-frame (fn)
    (handler-bind ((error (lambda (err)
                              (declare (ignore err))
                              (return-from get-test-frame
                                           (find-bomb-frame
                                               (alive/frames:list-debug-frames)
                                               (string fn))))))
        (funcall fn)))


(defun test-forms ()
    (clue:test "Bomb Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected (pos:create 13 10)
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-test-frame 'bomb)))))

    (clue:test "Bomb 2 Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected (pos:create 21 10)
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-test-frame 'bomb-2))))))


(defun run-all ()
    (clue:suite "Debugger Tests"
        (test-forms)))
