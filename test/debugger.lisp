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


(defun get-test-frame (fn &optional (limit nil))
    (handler-bind ((error (lambda (err)
                              (declare (ignore err))
                              (return-from get-test-frame
                                           (find-bomb-frame
                                               (if limit
                                                   (alive/frames:list-debug-frames limit)
                                                   (alive/frames:list-debug-frames))
                                               (string fn))))))
        (funcall fn)))


(defun get-step-frame (fn)
    (handler-bind ((error (lambda (err)
                              (declare (ignore err))
                              (return-from get-step-frame
                                           (find-bomb-frame
                                               (alive/frames:list-step-frames)
                                               (string fn))))))
        (funcall fn)))


(defun test-bomb ()
    (clue:test "Bomb Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected (pos:create 12 10)
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-test-frame 'bomb))))))


(defun test-bomb-step ()
    (clue:test "Bomb Step Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected (pos:create 12 10)
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-step-frame 'bomb))))))


(defun test-bomb-limit ()
    (clue:test "Bomb Limit Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected nil
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-test-frame 'bomb 1))))))


(defun test-bomb-2 ()
    (clue:test "Bomb 2 Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected (pos:create 20 10)
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-test-frame 'bomb-2))))))


(defun test-bomb-3 ()
    (clue:test "Bomb 3 Test"
        (with-open-file (file "test/debugger.lisp")
            (clue:check-equal :expected (pos:create 30 8)
                              :actual (alive/debugger:get-frame-loc
                                          file
                                          (get-test-frame 'bomb-3))))))


(defun test-no-index ()
    (clue:test "No index"
        (with-open-file (file "test/debugger.lisp")
            (let ((frame (get-test-frame 'bomb)))
                (setf (gethash "formNumber" frame) nil)
                (clue:check-equal :expected nil
                                  :actual (alive/debugger:get-frame-loc file
                                                                        frame))))))


(defun test-form-number ()
    (clue:test "No form number"
        (with-open-file (file "test/debugger.lisp")
            (let ((frame (get-test-frame 'bomb)))
                (setf (gethash "topForm" frame) nil)
                (clue:check-equal :expected nil
                                  :actual (alive/debugger:get-frame-loc file
                                                                        frame))))))


(defun run-all ()
    (clue:suite "Debugger Tests"
        (test-bomb)
        (test-bomb-step)
        (test-bomb-limit)
        (test-bomb-2)
        (test-bomb-3)
        (test-no-index)
        (test-form-number)))
