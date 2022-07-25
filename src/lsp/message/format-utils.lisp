(defpackage :alive/lsp/message/format-utils
    (:use :cl)
    (:export :to-text-edits)
    (:local-nicknames (:range :alive/range)
                      (:edit :alive/text-edit)))

(in-package :alive/lsp/message/format-utils)


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
