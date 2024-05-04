(defpackage :alive/utils
    (:use :cl)
    (:export :fuzzy-match
             :get-timestamp))

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


(defun get-timestamp ()
    (multiple-value-bind (sec minute hour day month year)
            (decode-universal-time (get-universal-time))
        (format nil "~d/~d/~d ~2,'0d:~2,'0d:~2,'0d" month day year hour minute sec)))


