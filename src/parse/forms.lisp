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
    (let ((form (form:create (token:get-start token)
                             (token:get-end token))))
        (form:set-token form token)
        form))


(defun from-stream (input)
    (loop :with forms := '()
          :with opens := '()

          :for token :in (tokenizer:from-stream input) :do

              (cond ((token:is-type types:*open-paren* token)
                     (push (form:create (token:get-start token))
                           opens))

                    ((token:is-type types:*close-paren* token)
                     (unless opens
                             (error (make-instance 'errors:input-error
                                                   :start (token:get-start token)
                                                   :end (token:get-end token)
                                                   :message "Unmatched close parenthesis")))

                     (let* ((open-form (pop opens)))
                         (form:set-end open-form (token:get-end token))

                         (if opens
                             (form:add-kid open-form (car opens))
                             (push open-form forms))))

                    ((token:is-type types:*ws* token) NIL)

                    (T (if opens
                           (form:add-kid (token-to-form token) (car opens))
                           (push (token-to-form token) forms))))

          :finally (return (reverse forms))))
