(defpackage :alive/lsp/message/document/format-utils
    (:use :cl)
    (:export :create-response
             :create-response-new)
    (:local-nicknames (:message :alive/lsp/message/abstract)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:edit :alive/text-edit)))

(in-package :alive/lsp/message/document/format-utils)


(defclass response (message:result-response)
        ())


(defclass text-edit ()
        ((range :accessor range
                :initform nil
                :initarg :range)
         (new-text :accessor new-text
                   :initform nil
                   :initarg :new-text)))


(defclass lsp-pos ()
        ((line :accessor line
               :initform nil
               :initarg :line)
         (character :accessor ch
                    :initform nil
                    :initarg :ch)))


(defclass lsp-range ()
        ((start :accessor start
                :initform nil
                :initarg :start)
         (end :accessor end
              :initform nil
              :initarg :end)))


(defun to-lsp-pos (pos)
    (make-instance 'lsp-pos
        :line (pos:line pos)
        :ch (pos:col pos)))


(defun to-lsp-range (range)
    (make-instance 'lsp-range
        :start (to-lsp-pos (range:start range))
        :end (to-lsp-pos (range:end range))))


(defun to-text-edits (edits)
    (if (and edits (< 0 (length edits)))
        (mapcar (lambda (edit)
                    (make-instance 'text-edit
                        :range (to-lsp-range (edit:range edit))
                        :new-text (edit:text edit)))
                edits)
        nil))


(defun create-response (id edits)
    (make-instance 'response
        :id id
        :result (to-text-edits edits)))


(defun create-response-new (id edits)
    (message:create-response id
                             :result-value (to-text-edits edits)))
