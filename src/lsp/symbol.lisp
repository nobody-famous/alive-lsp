(defpackage :alive/lsp/symbol
    (:use :cl)
    (:export :for-pos
             :for-document)
    (:local-nicknames (:form :alive/parse/form)
                      (:packages :alive/packages)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)
                      (:utils :alive/lsp/utils)))

(in-package :alive/lsp/symbol)


(defun for-pos (&key text pos)
    (let* ((pkg-name (packages:for-pos text pos))
           (pkg (packages:lookup pkg-name))
           (*package* (if pkg pkg *package*)))

        (multiple-value-bind (name pkg-name)
                (utils:symbol-for-pos text pos)
            (when (and name pkg-name)
                  (list name pkg-name)))))


(defparameter *kind-file* 1)
(defparameter *kind-module* 2)
(defparameter *kind-namespace* 3)
(defparameter *kind-package* 4)
(defparameter *kind-class* 5)
(defparameter *kind-method* 6)
(defparameter *kind-property* 7)
(defparameter *kind-field* 8)
(defparameter *kind-constructor* 9)
(defparameter *kind-enum* 10)
(defparameter *kind-interface* 11)
(defparameter *kind-function* 12)
(defparameter *kind-variable* 13)
(defparameter *kind-constant* 14)
(defparameter *kind-string* 15)
(defparameter *kind-number* 16)
(defparameter *kind-boolean* 17)
(defparameter *kind-array* 18)
(defparameter *kind-object* 19)
(defparameter *kind-key* 20)
(defparameter *kind-null* 21)
(defparameter *kind-enummember* 22)
(defparameter *kind-struct* 23)
(defparameter *kind-event* 24)
(defparameter *kind-operator* 25)
(defparameter *kind-typeparameter* 26)


(defun form-text (text form)
    (let* ((start (form:get-start-offset form))
           (end (form:get-end-offset form)))
        (subseq text start end)))


(defun form-kind (name)
    (let ((lc-name (string-downcase name)))
        (cond ((string= lc-name "defparameter") *kind-variable*)
              ((string= lc-name "defun") *kind-function*)
              ((string= lc-name "defmacro") *kind-function*)
              ((string= lc-name "defclass") *kind-class*)
              ((string= lc-name "defpackage") *kind-package*)
              ((string= lc-name "defstruct") *kind-struct*)
              ((string= lc-name "defmethod") *kind-method*))))


(defun to-doc-sym (name kind start end)
    (let ((doc-sym (make-hash-table :test #'equalp))
          (range (alive/range:create start (if end end start))))

        (setf (gethash "name" doc-sym) name)
        (setf (gethash "kind" doc-sym) kind)
        (setf (gethash "range" doc-sym) range)
        (setf (gethash "selectionRange" doc-sym) range)

        doc-sym))


(defun for-document (text forms)
    (loop :with syms := nil

          :for form :in forms :do
              (let* ((kids (form:get-kids form))
                     (fn (when (first kids)
                               (form-text text (first kids))))
                     (name (when (second kids)
                                 (form-text text (second kids))))
                     (kind (when fn (form-kind fn))))

                  (when (and fn name kind)
                        (push (to-doc-sym name kind (form:get-start form) (form:get-end form))
                              syms)))

          :finally (return syms)))
