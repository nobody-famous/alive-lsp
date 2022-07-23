(defpackage :alive/lsp/message/document/format-utils
    (:use :cl)
    (:export :create-response)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:range :alive/range)
                      (:edit :alive/text-edit)))

(in-package :alive/lsp/message/document/format-utils)


(defun to-lsp-pos (pos)
    (list (cons :line (cdr (assoc :line pos)))
          (cons :character (cdr (assoc :character pos)))))


(defun to-lsp-range (range)
    (list (cons :start (to-lsp-pos (range:start range)))
          (cons :end (to-lsp-pos (range:end range)))))


(defun to-text-edits (edits)
    (if (and edits (< 0 (length edits)))
        (mapcar (lambda (edit)
                    (list (cons :range (to-lsp-range (edit:range edit)))
                          (cons :new-text (edit:text edit))))
                edits)
        nil))


(defun create-response (id edits)
    (message:create-response id
                             :result-value (to-text-edits edits)))
