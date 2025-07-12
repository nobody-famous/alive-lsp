(defpackage :alive/utils
    (:use :cl)
    (:export :fuzzy-match
             :get-timestamp
             :lookup-symbol
             :url-encode-filename))

(in-package :alive/utils)


(defun fuzzy-match (pref str)
    (cond ((zerop (length pref)) T)
          ((zerop (length str)) NIL)
          (T (loop :with to-match := (elt pref 0)

                   :for ch :across str :do
                       (when (and (< 0 (length pref))
                                  (or (char= ch (char-upcase (elt pref 0)))
                                      (char= ch (char-downcase (elt pref 0)))))
                             (setf pref (subseq pref 1)))

                   :finally (return (and pref (= 0 (length pref))))))))


(declaim (ftype (function () string) get-timestamp))
(defun get-timestamp ()
    (multiple-value-bind (sec minute hour day month year)
            (decode-universal-time (get-universal-time))
        (format nil "~d/~d/~d ~2,'0d:~2,'0d:~2,'0d" month day year hour minute sec)))


(declaim (ftype (function (standard-char) boolean) needs-encoding))
(defun needs-encoding (char)
    (eq char #\:))


(declaim (ftype (function (standard-char) string) encode-char))
(defun encode-char (ch)
    (if (needs-encoding ch)
        (format nil "%~2,'0X" (char-code ch))
        (string ch)))


(declaim (ftype (function (string) string) url-encode))
(defun url-encode (str)
    (let ((chars (map 'list (lambda (char)
                                (encode-char char))
                     str)))
        (apply #'concatenate 'string chars)))


(declaim (ftype (function (string) string) url-encode-filename))
(defun url-encode-filename (name)
    (let* ((raw-pieces (uiop:split-string name :separator "/\\"))
           (pieces (mapcar (lambda (piece)
                               (if (string= piece "")
                                   ""
                                   (format NIL "/~A" (url-encode piece))))
                           raw-pieces)))
        (apply #'concatenate 'string pieces)))


(defun lookup-symbol (name &optional pkg-name)
    (let ((pkg (if pkg-name
                   (find-package (string-upcase pkg-name))
                   *package*))
          (sym-name (if (and name
                             (char= #\| (char name 0))
                             (char= #\| (char name (1- (length name)))))
                        (subseq name 1 (- (length name) 1))
                        name)))
        (if pkg
            (or (find-symbol sym-name pkg)
                (find-symbol (string-upcase sym-name) pkg))
            (values nil nil))))
