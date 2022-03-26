(defpackage :alive/parse/forms
    (:use :cl)
    (:export :from-stream)
    (:local-nicknames (:errors :alive/errors)
                      (:types :alive/types)
                      (:form :alive/parse/form)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/parse/forms)


(defstruct parse-state
    forms
    opens)


(defun token-to-form (token)
    (form:create (token:get-start token)
                 (token:get-end token)
                 (token:get-type-value token)))


(defun open-paren (state token)
    (push (form:create (token:get-start token)
                       nil
                       types:*open-paren*)
          (parse-state-opens state)))


(defun is-open-paren (open-form)
    (and open-form
         (= types:*open-paren* (form:get-form-type open-form))))


(defun is-symbol (open-form)
    (and open-form
         (= types:*symbol* (form:get-form-type open-form))))


(defun is-quote (open-form)
    (and open-form
         (or (= types:*quote* (form:get-form-type open-form))
             (= types:*back-quote* (form:get-form-type open-form)))))


(defun collapse-opens (state &optional target)
    (loop :with prev := nil

          :for cur := (car (parse-state-opens state)) :do
              (when cur
                    (when prev
                          (form:add-kid cur prev)
                          (form:set-end cur (form:get-end prev)))

                    (unless (eq (form:get-form-type cur) target)
                            (pop (parse-state-opens state))
                            (setf prev cur)))

          :while (and cur
                      (not (eq (form:get-form-type cur) target)))

          :finally (when (and prev
                              (not (parse-state-opens state)))
                         (push prev (parse-state-forms state)))))


(defun close-paren (state token)
    (collapse-opens state types:*open-paren*)

    (let ((open-form (pop (parse-state-opens state))))
        (unless (is-open-paren open-form)
                (error (make-instance 'errors:input-error
                                      :start (token:get-start token)
                                      :end (token:get-end token)
                                      :message "Unmatched close parenthesis")))

        (form:set-end open-form (token:get-end token))

        (let ((next-open (car (parse-state-opens state))))
            (cond ((is-quote next-open)
                   (form:add-kid next-open open-form)
                   (form:set-end next-open (form:get-end open-form))
                   (collapse-opens state types:*open-paren*))

                  ((is-open-paren next-open)
                   (form:add-kid (car (parse-state-opens state)) open-form))

                  (T (push open-form (parse-state-forms state)))))))


(defun start-quote (state token)
    (let ((open-form (car (parse-state-opens state))))
        (cond ((is-quote open-form) NIL)
              (T (push (form:create (token:get-start token) nil (token:get-type-value token))
                       (parse-state-opens state))))))


(defun symbol-token (state token)
    (let ((open-form (car (parse-state-opens state))))
        (cond ((or (is-open-paren open-form)
                   (is-quote open-form))
               (form:set-end open-form (token:get-end token))
               (push (form:create (token:get-start token)
                                  (token:get-end token)
                                  types:*symbol*)
                     (parse-state-opens state)))

              ((is-symbol open-form)
               (form:set-end open-form (token:get-end token)))

              (T (push (form:create (token:get-start token)
                                    (token:get-end token)
                                    types:*symbol*)
                       (parse-state-opens state))))))


(defun white-space (state)
    (collapse-opens state types:*open-paren*))


(defun from-stream (input)
    (loop :with state := (make-parse-state)

          :for token :in (tokenizer:from-stream input) :do

              (cond ((token:is-type types:*open-paren* token) (open-paren state token))

                    ((token:is-type types:*close-paren* token) (close-paren state token))

                    ((or (token:is-type types:*quote* token)
                         (token:is-type types:*back-quote* token)) (start-quote state token))

                    ((token:is-type types:*ws* token) (white-space state))

                    (T (symbol-token state token)))

          :finally (progn (collapse-opens state)
                          (return (reverse (parse-state-forms state))))))
