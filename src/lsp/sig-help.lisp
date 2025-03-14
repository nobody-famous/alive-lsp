(defpackage :alive/lsp/sig-help
    (:use :cl)
    (:export :signatures)
    (:local-nicknames (:forms :alive/parse/forms)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/lsp/sig-help)


(defun get-param-info (name)
    (let ((info (make-hash-table)))
        (setf (gethash "label" info) name)
        (setf (gethash "documentation" info) "Some param docs")
        info))


(defun get-sig-info (name)
    (let ((info (make-hash-table)))
        (setf (gethash "label" info) name)
        (setf (gethash "documentation" info) "This is some doc stuff")
        (setf (gethash "parameters" info) (list (get-param-info (list 4 5)) (get-param-info (list 6 7))))
        (setf (gethash "activeParameter" info) 1)
        info))


(defun signatures (&key text pos)
    (let* ((forms (forms:from-stream-or-nil (make-string-input-stream text)))
           (tokens (tokenizer:from-stream (make-string-input-stream text)))
           (top-form (forms:get-top-form forms pos))
           (outer-form (forms:get-outer-form top-form pos))
           (name-form (when (hash-table-p outer-form)
                            (first (gethash "kids" outer-form))))
           (name-tokens (when (hash-table-p name-form)
                              (alive/lsp/utils:find-tokens tokens (gethash "end" name-form)))))

        (format T "***** TOKENS~%")
        (loop :for token :in name-tokens
              :do (alive/test/utils:print-hash-table "***** TOKEN" token))
        (format T "***** END TOKENS~%")

        (list (get-sig-info "foo x y")
              (get-sig-info "foo a b"))))
