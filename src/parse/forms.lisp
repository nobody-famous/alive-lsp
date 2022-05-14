(defpackage :alive/parse/forms
    (:use :cl)
    (:export :from-stream)
    (:local-nicknames (:errors :alive/errors)
                      (:types :alive/types)
                      (:form :alive/parse/form)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/parse/forms)


(declaim (optimize (speed 3) (safety 0)))


(declaim (type fixnum
               types:*open-paren*
               types:*symbol*
               types:*quote*
               types:*back-quote*
               types:*comma*
               types:*comma-at*))


(defstruct parse-state
    forms
    opens)


(defun token-to-form (token)
    (form:create :start (token:get-start token)
                 :start-offset (token:get-start-offset token)
                 :end (token:get-end token)
                 :end-offset (token:get-end-offset token)
                 :form-type (token:get-type-value token)))


(defun open-paren (state token)
    (push (form:create :start (token:get-start token)
                       :start-offset (token:get-start-offset token)
                       :end nil
                       :form-type types:*open-paren*)
          (parse-state-opens state)))


(defun is-open-paren (open-form)
    (and open-form
        (= types:*open-paren* (the fixnum (form:get-form-type open-form)))))


(defun is-symbol (open-form)
    (and open-form
        (= types:*symbol* (the fixnum (form:get-form-type open-form)))))


(defun is-quote (open-form)
    (and open-form
        (or (= types:*quote* (the fixnum (form:get-form-type open-form)))
            (= types:*back-quote* (the fixnum (form:get-form-type open-form))))))


(defun is-comma (open-form)
    (and open-form
        (or (= (the fixnum types:*comma*) (the fixnum (form:get-form-type open-form)))
            (= (the fixnum types:*comma-at*) (the fixnum (form:get-form-type open-form))))))


(defun collapse-opens (state &optional target)
    (loop :with prev := nil

        :for cur := (car (parse-state-opens state)) :do
        (when cur
            (when prev
                (form:add-kid cur prev)
                (form:set-end cur (form:get-end prev))
                (form:set-end-offset cur (form:get-end-offset prev)))

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
        (form:set-end-offset open-form (token:get-end-offset token))

        (let ((next-open (car (parse-state-opens state))))
            (cond ((or (is-comma next-open)
                       (is-quote next-open))
                   (form:add-kid next-open open-form)
                   (form:set-end next-open (form:get-end open-form))
                   (form:set-end-offset next-open (form:get-end-offset open-form))
                   (collapse-opens state types:*open-paren*))

                ((is-open-paren next-open)
                 (form:add-kid (car (parse-state-opens state)) open-form))

                ((is-symbol next-open) nil)

                (T (push open-form (parse-state-forms state)))))))


(defun start-quote (state token)
    (let ((open-form (car (parse-state-opens state))))
        (cond ((is-quote open-form) NIL)
            (T (push (form:create :start (token:get-start token)
                                  :start-offset (token:get-start-offset token)
                                  :end nil
                                  :form-type (token:get-type-value token))
                     (parse-state-opens state))))))


(defun start-comma (state token)
    (let ((open-form (car (parse-state-opens state))))
        (cond ((is-comma open-form) NIL)
            (T (push (form:create :start (token:get-start token)
                                  :start-offset (token:get-start-offset token)
                                  :end nil
                                  :form-type (token:get-type-value token))
                     (parse-state-opens state))))))


(defun symbol-token (state token)
    (let ((open-form (car (parse-state-opens state))))

        (cond ((or (is-open-paren open-form)
                   (is-quote open-form))
               (when (string= "in-package" (string-downcase (the simple-string (token:get-text token))))
                   (form:set-is-in-pkg open-form T))
               (form:set-end open-form (token:get-end token))
               (form:set-end-offset open-form (token:get-end-offset token))
               (push (form:create :start (token:get-start token)
                                  :start-offset (token:get-start-offset token)
                                  :end (token:get-end token)
                                  :end-offset (token:get-end-offset token)
                                  :form-type types:*symbol*
                                  :in-pkg (form:is-in-pkg open-form))
                     (parse-state-opens state)))

            ((is-symbol open-form)
             (form:set-end open-form (token:get-end token))
             (form:set-end-offset open-form (token:get-end-offset token)))

            (T (push (form:create :start (token:get-start token)
                                  :start-offset (token:get-start-offset token)
                                  :end (token:get-end token)
                                  :end-offset (token:get-end-offset token)
                                  :form-type types:*symbol*)
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

            ((or (token:is-type types:*comma* token)
                 (token:is-type types:*comma-at* token)) (start-comma state token))

            ((token:is-type types:*ws* token) (white-space state))

            ((or (token:is-type types:*line-comment* token)
                 (token:is-type types:*block-comment* token)
                 (token:is-type types:*ifdef-true* token))
             NIL)

            ((token:is-type types:*ifdef-false* token)
             (if (parse-state-opens state)
                 (form:add-kid (car (parse-state-opens state))
                               (form:create :start (token:get-start token)
                                            :start-offset (token:get-start-offset token)
                                            :end (token:get-end token)
                                            :end-offset (token:get-end-offset token)
                                            :form-type types:*ifdef-false*))
                 (push (form:create :start (token:get-start token)
                                    :start-offset (token:get-start-offset token)
                                    :end (token:get-end token)
                                    :end-offset (token:get-end-offset token)
                                    :form-type types:*ifdef-false*)
                       (parse-state-forms state))))

            (T (symbol-token state token)))

        :finally (progn (collapse-opens state)
                     (return (reverse (parse-state-forms state))))))