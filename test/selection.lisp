(defpackage :alive/test/selection
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:selection :alive/selection)))

(in-package :alive/test/selection)


(defun test-basic ()
    (clue:test "Basic Selection"
        (let ((forms (forms:from-stream (make-string-input-stream (format nil "(foo bar (baz bong))~%(checking foo)"))))
              (pos-list (list (pos:create 0 12))))
            (clue:check-equal :expected #+n (list (list (range:create (pos:create 0 0) (pos:create 1 14))
                                                        (range:create (pos:create 0 0) (pos:create 0 20))
                                                        (range:create (pos:create 0 9) (pos:create 0 19))
                                                        (range:create (pos:create 0 9) (pos:create 0 19))
                                                        (range:create (pos:create 0 10) (pos:create 0 13))
                                                        (range:create (pos:create 0 10) (pos:create 0 13))))
                              (list (cons :range (range:create (pos:create 0 10) (pos:create 0 13)))
                                    (cons :parent (list (cons :range (range:create (pos:create 0 9) (pos:create 0 19)))
                                                        (cons :parent (list (cons :range (range:create (pos:create 0 0) (pos:create 0 20)))
                                                                            (cons :parent (list (cons :range (range:create (pos:create 0 0) (pos:create 1 14)))
                                                                                                (cons :parent nil))))))))
                              :actual #+n (selection:ranges forms pos-list)
                              (selection::get-range-tree forms (first pos-list))))))


(list (cons :range (range:create (pos:create 0 10) (pos:create 0 13)))
      (cons :parent (list (cons :range (range:create (pos:create 0 9) (pos:create 0 19)))
                          (cons :parent (list (cons :range (range:create (pos:create 0 0) (pos:create 0 20)))
                                              (cons :parent (list (cons :range (range:create (pos:create 0 0) (pos:create 1 14)))
                                                                  (cons :parent nil))))))))


(defun no-forms ()
    (clue:test "Selection No Forms"
        (let ((forms (forms:from-stream (make-string-input-stream "foo")))
              (pos-list (list (pos:create 0 2))))
            (selection:ranges forms pos-list))))


(defun run-all ()
    (clue:suite "Selection Tests"
        (test-basic)))
