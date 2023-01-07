(defpackage :alive/lsp/message/format-utils
    (:use :cl)
    (:export :to-text-edits)
    (:local-nicknames (:range :alive/range)
                      (:edit :alive/text-edit)))

(in-package :alive/lsp/message/format-utils)


(defun to-lsp-pos (pos)
    (let ((lsp-pos (make-hash-table :test #'equalp)))

        (setf (gethash "line" lsp-pos) (cdr (assoc :line pos)))
        (setf (gethash "character" lsp-pos) (cdr (assoc :character pos)))

        lsp-pos))


(defun to-lsp-range (range)
    (let ((lsp-range (make-hash-table :test #'equalp)))

        (setf (gethash "start" lsp-range) (range:start range))
        (setf (gethash "end" lsp-range) (range:end range))

        lsp-range))


(defun to-text-edits (edits)
    (if (and edits (< 0 (length edits)))
        (mapcar (lambda (edit)
                    (let ((text-edit (make-hash-table :test #'equalp)))

                        (setf (gethash "range" text-edit) (to-lsp-range (edit:range edit)))
                        (setf (gethash "newText" text-edit) (edit:text edit))

                        text-edit))
                edits)
        (make-array 0)))
