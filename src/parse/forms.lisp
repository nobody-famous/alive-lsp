(defpackage :alive/parse/forms
    (:use :cl)
    (:export :from-stream)
    (:local-nicknames (:errors :alive/errors)
                      (:types :alive/types)
                      (:form :alive/parse/form)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/parse/forms)


(defun token-to-form (token)
    (form:create (token:get-start token)
                 (token:get-end token)))


(defun from-stream (input)
    (loop :with forms := '()
          :with opens := '()

          :for token :in (tokenizer:from-stream input) :do

              (format T "token ~A~%" token)
              (cond ((token:is-type types:*open-paren* token) (push token opens))

                    ((token:is-type types:*close-paren* token)
                     (unless opens
                             (error (make-instance 'errors:input-error
                                                   :start (token:get-start token)
                                                   :end (token:get-end token)
                                                   :message "Unmatched close parenthesis")))

                     (let* ((open-paren (pop opens))
                            (new-form (form:create (token:get-start open-paren)
                                                   (token:get-end token))))
                         (if opens
                             (form:add-kid new-form (car opens))
                             (push new-form forms))))

                    ((token:is-type types:*ws* token) NIL)

                    (T (if opens
                           (form:add-kid (token-to-form token) (car opens))
                           (push (token-to-form token) forms))))

          :finally (return (reverse forms))))
