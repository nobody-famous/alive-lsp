(defpackage :alive/format
    (:use :cl)
    (:export :range)
    (:local-nicknames (:edit :alive/text-edit)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)))

(in-package :alive/format)


(defstruct parse-state
    tokens
    (indent (list 0))
    edits
    out-list)


(defun next-token (state)
    (car (parse-state-tokens state)))


(defun pop-token (state)
    (setf (parse-state-tokens state) (cdr (parse-state-tokens state))))


(defun add-to-out-list (state token)
    (setf (parse-state-out-list state) (cons token (parse-state-out-list state))))


(defun out-of-range (range token)
    (or (pos:less-or-equal (token:get-end token) (range:start range))
        (pos:less-than (range:end range) (token:get-start token))))


(defun process-open (state token)
    (let* ((end (token:get-end token)))
        (add-to-out-list state token)
        (setf (parse-state-indent state) (cons (pos:col end) (parse-state-indent state)))))


(defun process-close (state token)
    (let* ((end (token:get-end token)))
        (add-to-out-list state token)
        (setf (parse-state-indent state) (cdr (parse-state-indent state)))))


(defun replace-token (state token text)
    (let* ((range (range:create (token:get-start token)
                                (token:get-end token)))
           (edit (edit:create :range range
                              :text text)))
        (setf (parse-state-edits state)
              (cons edit (parse-state-edits state)))))


(defun process-ws (state token)
    (let ((prev (car (parse-state-out-list state))))
        (cond ((or (not prev)
                   (= types:*open-paren* (token:get-type-value prev)))
               (replace-token state token ""))

              (() ())

              (() ()))

        (format T "ws ~A ~A ~A ~A~%"
                (length (parse-state-tokens state))
                prev
                (token:get-start token)
                (token:get-end token))))


(defun range (input range)
    (let* ((tokens (tokenizer:from-stream input))
           (state (make-parse-state :tokens tokens)))

        (loop :while (parse-state-tokens state)
              :do (let ((token (next-token state)))
                      (cond ((= types:*open-paren* (token:get-type-value token)) (process-open state token))
                            ((= types:*close-paren* (token:get-type-value token)) (process-close state token))
                            ((= types:*ws* (token:get-type-value token)) (process-ws state token))
                            (T (add-to-out-list state token)))
                      (pop-token state))
              :finally (progn (format T "end state ~A~%" state)
                              (return (reverse (parse-state-edits state)))))))
