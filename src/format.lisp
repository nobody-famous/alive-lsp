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


(defparameter *start-form* 100)


(defclass start-form ()
    ((start :accessor start
            :initform nil
            :initarg :start)
     (end :accessor end
          :initform nil
          :initarg :end)
     (is-multiline :accessor is-multiline
                   :initform nil
                   :initarg :is-multiline)))


(defun new-start-form (token)
    (make-instance 'start-form
                   :start (token:get-start token)
                   :end (token:get-end token)
                   :is-multiline nil))


(defmethod token:get-type-value ((obj start-form))
    *start-form*)


(defmethod token:get-start ((obj start-form))
    (start obj))


(defmethod token:get-end ((obj start-form))
    (end obj))


(defstruct parse-state
    tokens
    (indent (list 0))
    edits
    out-list)


(defun next-token (state)
    (car (parse-state-tokens state)))


(defun pop-token (state)
    (pop (parse-state-tokens state)))


(defun add-to-out-list (state token)
    (push token (parse-state-out-list state)))


(defun out-of-range (range token)
    (or (pos:less-or-equal (token:get-end token) (range:start range))
        (pos:less-than (range:end range) (token:get-start token))))


(defun process-open (state token)
    (let* ((end (token:get-end token)))
        (add-to-out-list state token)
        (push (pos:col end) (parse-state-indent state))))


(defun process-close (state token)
    (let* ((end (token:get-end token)))
        (add-to-out-list state token)
        (pop (parse-state-indent state))))


(defun replace-token (state token text)
    (let* ((range (range:create (token:get-start token)
                                (token:get-end token)))
           (edit (edit:create :range range
                              :text text)))
        (push edit (parse-state-edits state))))


(defun process-ws (state token)
    (let ((prev (car (parse-state-out-list state))))
        (cond ((or (not prev)
                   (= *start-form* (token:get-type-value prev)))
               (replace-token state token ""))

              (() ())

              (() ()))))


(defun convert-tokens (tokens)
    (loop :with converted := '()
          :with opens = '()

          :for token :in tokens :do
              (cond ((= types:*open-paren* (token:get-type-value token))
                     (let ((new-token (new-start-form token)))
                         (push new-token opens)
                         (push new-token converted)))

                    (() ())

                    (T (push token converted)))

          :finally (return (reverse converted))))


(defun range (input range)
    (let* ((tokens (convert-tokens (tokenizer:from-stream input)))
           (state (make-parse-state :tokens tokens)))

        (loop :while (parse-state-tokens state)

              :do (let ((token (next-token state)))
                      (format T "~A~%" token)
                      (cond ((= *start-form* (token:get-type-value token)) (process-open state token))
                            ((= types:*close-paren* (token:get-type-value token)) (process-close state token))
                            ((= types:*ws* (token:get-type-value token)) (process-ws state token))
                            (T (add-to-out-list state token)))
                      (pop-token state))

              :finally (return (reverse (parse-state-edits state))))))
