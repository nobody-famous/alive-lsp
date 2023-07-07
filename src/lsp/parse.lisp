(defpackage :alive/lsp/parse
    (:use :cl)
    (:export :from-stream)

    (:local-nicknames (:packet :alive/lsp/packet)))

(in-package :alive/lsp/parse)


(defvar *return-char* (char-code #\return))
(defvar *nl-char* (char-code #\newline))


(defun trim-ws (str)
    (string-trim (list #\space #\newline #\linefeed #\return)
                 str))


(defun next-line (input)
    (loop :with out := (flexi-streams:make-in-memory-output-stream)
          :with prev-char := 0

          :until (and (eq *return-char* prev-char)
                      (eq *nl-char* (flexi-streams:peek-byte input)))

          :do (let ((ch (read-byte input)))
                  (setf prev-char ch)
                  (write-byte ch out))

          :finally (progn (read-byte input)
                          (return (trim-ws (flexi-streams:octets-to-string
                                               (flexi-streams:get-output-stream-sequence out)))))))


(defun get-header-lines (input)
    (loop :with done := nil
          :with lines := ()
          :until done
          :do (let ((line (next-line input)))
                  (if (zerop (length line))
                      (setf done T)
                      (setf lines (cons line lines))))
          :finally (return lines)))


(defun parse-header-line (line)
    (let ((ndx (position #\: line)))
        (unless ndx (error (format nil "Invalid header line: ~A" line)))

        (list (trim-ws (subseq line 0 ndx))
              (trim-ws (subseq line (+ 1 ndx))))))


(defun add-to-header (header pair)
    (destructuring-bind (key value)
            pair
        (when (string= key "Content-Length")
              (setf (packet:content-length header)
                  (parse-integer value)))))


(defun parse-header (input)
    (loop :with header := (packet:create-header)
          :for line :in (get-header-lines input) :do
              (add-to-header header
                             (parse-header-line line))
          :finally (return header)))


(defun read-content-bytes (input size)
    (flexi-streams:with-output-to-sequence (out)
        (loop :for ndx :from 0 :below size :do
                  (let ((ch (read-byte input)))
                      (when ch (write-byte ch out))))))

(defun read-content (input size)
    (let ((content (flexi-streams:octets-to-string
                       (read-content-bytes input size)
                       :external-format :utf-8)))
        content))


(defun decode-json (content)
    (with-input-from-string (str content)
        (json:decode-json str)))


(defun from-stream (input)
    (let* ((header (parse-header input))
           (raw-content (read-content input (packet:content-length header)))
           (content (decode-json raw-content)))
        content))
