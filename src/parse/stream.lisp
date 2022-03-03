(defpackage :alive/parse/stream
    (:use :cl)
    (:export :from)
    (:local-nicknames (:errors :alive/errors)))

(in-package :alive/parse/stream)


(defparameter *open-parens* #\()
(defparameter *close-parens* #\))


(defun not-ws (ch)
    (and (not (char= ch #\space))
         (graphic-char-p ch)))


(defun look-ahead (input)
    (peek-char nil input nil nil t))


(defun read-next (input)
    (read-preserving-whitespace input nil nil))


(defun discard (input &optional expected)
    (let ((ch (read-char input nil nil)))
        (unless ch
                (error (format nil "End of input")))

        (when (and expected
                   (not (char= ch expected)))
              (error (format nil "Expected ~A, found ~A" expected ch)))))


(defun skip-ws (input)
    (loop :until (or (not (look-ahead input))
                     (not-ws (look-ahead input)))
          :do (discard input)))


(defun parse-expr (input)
    (flet ((parse-list ()
                (discard input *open-parens*)
                (skip-ws input)

                (loop :until (or (not (look-ahead input))
                                 (char= (look-ahead input) *close-parens*))
                      :collect (parse-expr input) :into parts
                      :finally (progn (discard input *close-parens*)
                                      (return parts))))

           (parse-atom ()
                (read-next input)))

        (skip-ws input)

        (when (look-ahead input)
              (let* ((start (file-position input))
                     (expr (handler-case
                                   (cond ((char= (look-ahead input) *open-parens*) (parse-list))
                                         (t (parse-atom)))
                               (T (c)
                                  (error (make-instance 'errors:input-error
                                                        :start start
                                                        :end (file-position input)
                                                        :message (format nil "~A" c))))))
                     (end (1- (file-position input))))
                  (skip-ws input)
                  (list start end expr)))))


(defun from (input)
    (loop :with start := (file-position input)
          :while (look-ahead input)
          :collect (parse-expr input) :into exprs
          :finally (return (list start (file-position input) exprs))))