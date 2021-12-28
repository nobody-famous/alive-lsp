(defpackage :alive/parse/tokenizer
    (:use :cl)
    (:export :from-stream)
    (:local-nicknames (:pos :alive/parse/pos)
                      (:token :alive/parse/token)
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

        (token:create :type-value tok-type
                      :start (token-start state)
                      :end end
                      :text text)))


(defun read-ws-token (state)
    (loop :with str := (make-string-output-stream)
          :with next-ch := (look-ahead state)

          :while (and next-ch (is-ws next-ch))
          :do (write-char (next-char state) str)
              (setf next-ch (look-ahead state))

          :finally (return (new-token state types:*ws* (get-output-stream-string str)))))


(defun read-symbol-token (state)
    (let ((start (file-position (input state)))
          (sym (read-preserving-whitespace (input state) nil nil))
          (end (file-position (input state))))

        (incf (col state) (- end start))
        (new-token state types:*symbol* sym)))


(defun read-ch-token (state tok-type text)
    (next-char state)
    (new-token state tok-type text))


(defun next-token (state)
    (setf (token-start state)
          (pos:create :line (line state) :col (col state)))

    (let ((ch (look-ahead state)))
        (cond ((char= ch #\() (read-ch-token state types:*open-paren* "("))
              ((char= ch #\)) (read-ch-token state types:*close-paren* ")"))
              ((is-ws ch) (read-ws-token state))
              (T (read-symbol-token state)))))


(defun from-stream (input)
    (loop :with state := (make-instance 'parse-state :input input)
          :while (look-ahead state)
          :collect (next-token state)))
