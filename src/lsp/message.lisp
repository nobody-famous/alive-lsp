(defpackage :alive/lsp/message
    (:use :cl)
    (:export :parse)
)

(in-package :alive/lsp/message)


(defstruct header
    (content-length 0)
)


(defstruct base-message
    header
    content
)


(defun trim-ws (str)
    (string-trim (list #\space #\newline #\linefeed #\return)
                 str
    ))


(defun next-line (input)
    (loop :with str := (make-string-output-stream)
          :with prev-char := (code-char 0)
          :until (and (char= #\return prev-char)
                      (char= #\newline
                             (peek-char nil input nil nil)
                      ))
          :do (let ((ch (read-char input)))
                  (setf prev-char ch)
                  (write-char ch str)
              )
          :finally (progn (read-char input)
                          (return (trim-ws (get-output-stream-string str)))
                   )))


(defun get-header-lines (input)
    (loop :with done := nil
          :with lines := ()
          :until done
          :do (let ((line (next-line input)))
                  (if (zerop (length line))
                      (setf done T)
                      (setf lines (cons line lines))
                  ))
          :finally (progn
                    (return lines)
                   )))


(defun parse-header-line (line)
    (let ((ndx (position #\: line)))
        (unless ndx (error (format nil "Invalid header line: ~A" line)))

        (list (trim-ws (subseq line 0 ndx))
              (trim-ws (subseq line (+ 1 ndx)))
        )))


(defun add-to-header (header pair)
    (destructuring-bind (key value)
            pair
        (cond ((string= key "Content-Length") (setf (header-content-length header)
                                                    (parse-integer value)
                                              ))
              (T (error (format nil "Unhandled header key: ~A" key)))
        )))


(defun parse-header (input)
    (loop :with header := (make-header)
          :for line :in (get-header-lines input) :do
              (add-to-header header
                             (parse-header-line line)
              )
          :finally (return header)
    ))


(defun read-content (input size)
    (with-output-to-string (out)
        (loop :for ndx :from 0 :below size :do
                  (let ((ch (read-char input nil nil)))
                      (when ch (write-char ch out))
                  ))))


(defun decode-content (content)
    (with-input-from-string (str content)
        (json:decode-json str)
    ))


(defun parse (input)
    (let* ((header (parse-header input))
           (raw-content (read-content input (header-content-length header)))
           (content (decode-content raw-content))
          )
        (format T "content ~A~%" content)
    ))
