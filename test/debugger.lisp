(defpackage :alive/test/debugger
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:debugger :alive/debugger)
                      (:pos :alive/position)))

(in-package :alive/test/debugger)


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


(defun bomb-3 ()
    (let* ((result (alive/eval:from-string "()"
                                           :pkg-name "cl-user"
                                           :stdin-fn (lambda () nil)
                                           :stdout-fn (lambda () nil)
                                           :stderr-fn (lambda () nil))))
        (declare (ignore result))
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


(defun test-bomb ()
    (clue:test "Bomb Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected (pos:create 13 10)
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-test-frame 'bomb))))))


(defun test-bomb-2 ()
    (clue:test "Bomb 2 Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected (pos:create 21 10)
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-test-frame 'bomb-2))))))


(defun test-bomb-3 ()
    (clue:test "Bomb 3 Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected (pos:create 31 8)
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-test-frame 'bomb-3))))))


(defun run-all ()
    (clue:suite "Debugger Tests"
        (test-bomb)
        (test-bomb-2)
        (test-bomb-3)))
