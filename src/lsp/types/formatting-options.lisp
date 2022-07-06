(defpackage :alive/lsp/types/formatting-options
    (:use :cl)
    (:export :from-wire)
    (:local-nicknames (:types :alive/types)
                      (:fmt-opts :alive/lsp/types/format-options)))

(in-package :alive/lsp/types/formatting-options)


(defun from-wire (results)
    (labels ((add-param (params key value)
                        (cond ((eq key :tab-size) (setf (fmt-opts::indent-width params) value)))))

        (loop :with item := (make-instance 'fmt-opts::format-options)
              :for param :in results :do
                  (add-param item (car param) (cdr param))
              :finally (return item))))