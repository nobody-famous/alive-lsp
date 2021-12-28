(defpackage :alive/parse/tokenizer
    (:use :cl)
    (:export :from-stream)
    (:local-nicknames (:pos :alive/parse/pos)
                      (:types :alive/types)))

(in-package :alive/parse/tokenizer)


(defclass parse-state ()
    ((input :accessor input
            :initform nil
            :initarg :input)
     (token-start :accessor token-start
                  :initform nil
                  :initarg :token-start)
     (line :accessor line
           :initform 0
           :initarg :line)
     (col :accessor col
          :initform 0
          :initarg :col)))


(defun look-ahead (state)
    (peek-char nil (input state) nil nil t))


(defun is-ws (ch)
    (or (char= ch #\space)
        (char= ch #\tab)
        (char= ch #\return)
        (char= ch #\newline)))


(defun next-char (state)
    (let ((ch (read-char (input state) nil nil)))
        (cond ((char= ch #\newline) (incf (line state))
                                    (setf (col state) 0))
              (T (incf (col state))))

        ch))


(defun new-token (state tok-type text)
    (let ((end (pos:create :line (line state)
                           :col (col state))))

        (format T "NEW TOKEN ~A ~A ~A ~A~%"
                tok-type
                (token-start state)
                end
                text)))


(defun read-ws-token (state)
    (loop :with str := (make-string-output-stream)
          :with next-ch := (look-ahead state)

          :while (and next-ch (is-ws next-ch))
          :do (write-char (next-char state) str)
              (setf next-ch (look-ahead state))

          :finally (new-token state types:*ws* (get-output-stream-string str))))


(defun next-token (state)
    (setf (token-start state)
          (pos:create :line (line state) :col (col state)))

    (let ((ch (next-char state)))
        (cond ((char= ch #\() (new-token state types:*open-paren* "("))
              ((char= ch #\)) (new-token state types:*close-paren* ")"))
              ((is-ws ch) (read-ws-token state)))))


(defun from-stream (input)
    (loop :with state := (make-instance 'parse-state :input input)
          :while (look-ahead state)
          :do (next-token state)))
