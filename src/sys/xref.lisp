(defpackage :alive/sys/xref
    (:use :cl)
    (:export :get-locations)
    (:local-nicknames (:loc :alive/location)
                      (:pos :alive/position)
                      (:range :alive/range)
                      (:sym :alive/symbols)
                      (:token :alive/parse/token)
                      (:utils :alive/utils)))

(in-package :alive/sys/xref)


(declaim (ftype (function (string string) *) find-callers))
(defun find-callers (name pkg-name)
    (let ((to-find (sym:lookup name pkg-name)))
        (cond ((sym:function-p name pkg-name) (sb-introspect:who-calls to-find))
              ((sym:macro-p name pkg-name) (sb-introspect:who-macroexpands to-find))
              (T (sb-introspect:who-references to-find)))))


(declaim (ftype (function (cons) (or null cons)) caller-to-location))
(defun caller-to-location (caller)
    (let* ((path (sb-introspect:definition-source-pathname (cdr caller)))
           (path-str (if path
                         (namestring (sb-introspect:definition-source-pathname (cdr caller)))
                         nil)))
        (list (cons :file path-str)
              (cons :offset (sb-introspect:definition-source-character-offset (cdr caller))))))


(declaim (ftype (function (string string) (or null cons)) lookup-references))
(defun lookup-references (name pkg-name)
    (let* ((locations (mapcar #'caller-to-location (find-callers name pkg-name))))
        (remove-if-not (lambda (caller)
                           (stringp (cdr (assoc :file caller))))
                locations)))


(declaim (ftype (function (string) (values list &optional)) read-file-tokens))
(defun read-file-tokens (file)
    (with-open-file (s file)
        (alive/parse/tokenizer:from-stream s)))


(declaim (ftype (function (cons) hash-table) get-file-tokens))
(defun get-file-tokens (refs)
    (loop :with file := nil
          :with tokens := (make-hash-table :test #'equalp)
          :for ref :in refs
          :do (setf file (cdr (assoc :file ref)))
              (unless (gethash file tokens)
                  (setf (gethash file tokens) (read-file-tokens file)))
          :finally (return tokens)))


(declaim (ftype (function (hash-table cons) (values loc:text-location &optional)) ref-to-location))
(defun ref-to-location (tokens ref)
    (loop :with file := (cdr (assoc :file ref))
          :with offset := (cdr (assoc :offset ref))
          :for token :in (gethash file tokens)
          :while (< (token:get-end-offset token) offset)
          :finally (return (loc:create (utils:url-encode-filename file)
                                       (range:create (token:get-start token)
                                                     (token:get-end token))))))


(declaim (ftype (function (string string) (or null cons)) find-references))
(defun find-references (name pkg-name)
    (let* ((refs (lookup-references name pkg-name))
           (file-tokens (get-file-tokens refs)))
        (format T "***** REFS ~A~%" (mapcar (lambda (ref)
                                                (ref-to-location file-tokens ref))
                                            refs))
        (mapcar (lambda (ref)
                    (ref-to-location file-tokens ref))
                refs)))


(declaim (ftype (function (string pos:text-position) (or null cons)) get-locations))
(defun get-locations (text pos)
    (multiple-value-bind (name pkg-name)
            (sym:for-pos text pos)
        (when (and name pkg-name)
              (find-references name pkg-name))))
