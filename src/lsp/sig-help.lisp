(defpackage :alive/lsp/sig-help
    (:use :cl)
    (:export :signatures)
    (:local-nicknames (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:pkgs :alive/packages)
                      (:pos :alive/position)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)))

(in-package :alive/lsp/sig-help)


(defun get-param-info (label)
    (let ((info (make-hash-table :test #'equalp)))
        (setf (gethash "label" info) label)
        info))


(defun generate-label (fn-name pkg-name)
    (loop :with label := fn-name
          :with lambda-list := (symbols:get-lambda-list fn-name pkg-name)
          :with params := ()
          :with index := (length label)
          :with add-params := T

          :for item :in lambda-list
          :do (let ((item-str (format nil "~A" item)))
                  (when (char= #\& (char item-str 0))
                        (setf add-params nil))
                  (when add-params
                        (setf params (push (get-param-info (list (+ 1 index)
                                                                 (+ 1 index (length item-str))))
                                           params))
                        (setf index (+ 1 index (length item-str))))
                  (setf label (format nil "~A ~A" label item)))

          :finally (return (values label (or (reverse params) (make-array 0))))))


(defun get-sig-info (active-param fn-name pkg-name)
    (multiple-value-bind (label params)
            (generate-label fn-name pkg-name)
        (when (or (zerop (length params))
                  (< active-param (length params)))
              (let ((doc (or (documentation (symbols:lookup fn-name pkg-name) 'function) ""))
                    (info (make-hash-table :test #'equalp)))
                  (setf (gethash "label" info) label)
                  (setf (gethash "documentation" info) doc)
                  (setf (gethash "parameters" info) params)
                  (setf (gethash "activeParameter" info) active-param)
                  info))))


(defun xyz-get-active-parameter (pos form)
    (let* ((start (car (form:xyz-get-kids form)))
           (start-end (form:xyz-get-end start)))
        (if (pos:less-than pos start-end)
            -1
            (loop :with param := 0
                  :for kid :in (cdr (form:xyz-get-kids form))
                  :until (and (form:xyz-get-end kid)
                              (pos:less-than pos (form:xyz-get-end kid)))
                  :do (incf param)
                  :finally (return param)))))


(defun signatures (&key text pos)
    (let* ((forms (forms:xyz-from-stream-or-nil (make-string-input-stream text)))
           (top-form (forms:xyz-get-top-form forms pos))
           (outer-form (forms:xyz-get-outer-form top-form pos))
           (name-form (when outer-form
                            (first (form:xyz-get-kids outer-form))))
           (active-param (if outer-form
                             (xyz-get-active-parameter pos outer-form)
                             0))
           (name-tokens (when name-form (form:xyz-get-tokens name-form)))
           (pkg-name (alive/packages:for-pos text pos)))

        (when (and name-tokens
                   (<= 0 active-param))
              (multiple-value-bind (name pkg)
                      (pkgs:for-tokens name-tokens pkg-name)
                  (let ((sig (get-sig-info active-param name pkg)))
                      (when sig (list sig)))))))
