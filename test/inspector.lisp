(defpackage :alive/test/inspector
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:inspector :alive/inspector)
                      (:eval :alive/eval)))

(in-package :alive/test/inspector)


(defun fn-test ()
    (clue:test "Inspect Function Test"
        (let* ((result (eval:from-string "'defun"
                                         :pkg-name "cl-user"))
               (insp-result (inspector:to-result result)))

            (clue:check-equal :expected T
                              :actual (hash-table-p insp-result))
            (clue:check-equal :expected 4
                              :actual (hash-table-count insp-result)))))


(defun number-test ()
    (clue:test "Inspect Number Test"
        (let* ((result (eval:from-string "5"
                                         :pkg-name "cl-user"))
               (insp-result (inspector:to-result result)))

            (clue:check-equal :expected T
                              :actual (hash-table-p insp-result))
            (clue:check-equal :expected 2
                              :actual (hash-table-count insp-result)))))


(defun hashmap-test ()
    (clue:test "Inspect Hashmap Test"
        (let ((data (make-hash-table :test #'equalp)))
            (setf (gethash "foo" data) 1)
            (setf (gethash "bar" data) "Something")
            (setf (gethash "baz" data) (lambda () nil))

            (let* ((insp-result (inspector:to-result data)))
                (clue:check-equal :expected T
                                  :actual (hash-table-p insp-result))
                (clue:check-equal :expected 6
                                  :actual (hash-table-count insp-result))))))


(defun alist-test ()
    (clue:test "Inspect Association List Test"
        (let ((data (list (cons 'a 1)
                          (cons 'b (lambda () nil))
                          (cons 'c 3))))

            (let* ((insp-result (inspector:to-result data)))
                (clue:check-equal :expected T
                                  :actual (hash-table-p insp-result))
                (clue:check-equal :expected 3
                                  :actual (hash-table-count insp-result))))))


(defun string-list-test ()
    (clue:test "Inspect String List Test"
        (let ((data (list "First"
                          "Second"
                          "Third")))

            (let* ((insp-result (inspector:to-result data)))
                (clue:check-equal :expected T
                                  :actual (hash-table-p insp-result))
                (clue:check-equal :expected 3
                                  :actual (hash-table-count insp-result))))))


(defun vector-test ()
    (clue:test "Inspect Vector Test"
        (let ((data (make-array 0 :fill-pointer 0)))

            (vector-push-extend "First" data)
            (vector-push-extend "Second" data)
            (vector-push-extend "Third" data)

            (let* ((insp-result (inspector:to-result data)))
                (clue:check-equal :expected T
                                  :actual (hash-table-p insp-result))
                (clue:check-equal :expected 3
                                  :actual (hash-table-count insp-result))))))


(defun array-test ()
    (clue:test "Inspect Array Test"
        (let ((data (make-array 3
                        :fill-pointer nil
                        :adjustable nil
                        :displaced-to nil)))

            (setf (aref data 0) "First")
            (setf (aref data 1) "Second")
            (setf (aref data 2) "Third")

            (let* ((insp-result (inspector:to-result data)))
                (clue:check-equal :expected T
                                  :actual (hash-table-p insp-result))
                (clue:check-equal :expected 3
                                  :actual (hash-table-count insp-result))))))


(defun function-test ()
    (clue:test "Function Test"
        (let ((insp-result (inspector:to-result (alive/symbols:lookup "mapc" "cl-user"))))
            (clue:check-equal :expected T
                              :actual (hash-table-p insp-result))
            (clue:check-equal :expected 4
                              :actual (hash-table-count insp-result)))))


(defun symbol-test ()
    (clue:test "Symbol Test"
        (let ((insp-result (inspector:to-result (alive/symbols:lookup "foo" "cl-user"))))
            (clue:check-equal :expected T
                              :actual (hash-table-p insp-result))
            (clue:check-equal :expected 2
                              :actual (hash-table-count insp-result)))))


(defun cons-test ()
    (clue:test "Cons Test"
        (let ((insp-result (inspector:to-result (cons 5 10))))
            (clue:check-equal :expected T
                              :actual (hash-table-p insp-result))
            (clue:check-equal :expected 2
                              :actual (hash-table-count insp-result)))))


(defun create-test ()
    (clue:test "Create Test"
        (let ((insp (inspector:create :text "nil" :pkg "cl-user" :result "result")))
            (clue:check-equal :expected "nil" :actual (inspector:get-text insp))
            (clue:check-equal :expected "cl-user" :actual (inspector:get-pkg insp))
            (clue:check-equal :expected "result" :actual (inspector:get-result insp)))))


(defun run-all ()
    (clue:suite "Inspector Tests"
        (fn-test)
        (number-test)
        (hashmap-test)
        (alist-test)
        (string-list-test)
        (vector-test)
        (array-test)
        (function-test)
        (symbol-test)
        (cons-test)
        (create-test)))
