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


(defun is-delim (ch)
    (or (not ch)
        (is-ws ch)
        (char= ch #\))
        (char= ch #\()))


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


(defun read-text-token (&key state tok-type predicate)
    (loop :with str := (make-string-output-stream)

          :for ch := (look-ahead state)
          :while (funcall predicate ch)
          :do (write-char ch str)
              (next-char state)

          :finally (return (new-token state tok-type (get-output-stream-string str)))))


(defun read-ws-token (state)
    (read-text-token :state state
                     :tok-type types:*ws*
                     :predicate (lambda (ch)
                                    (and ch (is-ws ch)))))


(defun read-symbol-token (state)
    (read-text-token :state state
                     :tok-type types:*symbol*
                     :predicate (lambda (ch)
                                    (not (or (is-delim ch)
                                             (char= ch #\:))))))


(defun read-ch-token (state tok-type text)
    (next-char state)
    (new-token state tok-type text))


(defun read-string-token (state)
    (labels ((try-read (state)
                  (handler-case
                          (read-preserving-whitespace (input state))
                      (error (c)
                             (declare (ignore c))
                             nil))))

        (let ((start (file-position (input state)))
              (str (try-read state))
              (end (file-position (input state))))

            ; In case the string spans multiple lines, reset to the start
            ; and read character by character to the end so the state gets
            ; updated.
            (file-position (input state) start)
            (loop :until (eq end (file-position (input state)))
                  :do (next-char state))

            (when str
                  (new-token state types:*string* str)))))


(defun read-comment-token (state)
    (read-text-token :state state
                     :tok-type types:*comment*
                     :predicate (lambda (ch)
                                    (not (or (not ch)
                                             (char= #\newline ch))))))


(defun read-colons-token (state)
    (read-text-token :state state
                     :tok-type types:*colons*
                     :predicate (lambda (ch)
                                    (char= ch #\:))))


(defun next-token (state)
    (setf (token-start state)
          (pos:create :line (line state) :col (col state)))

    (let ((ch (look-ahead state)))
        (cond ((char= ch #\() (read-ch-token state types:*open-paren* "("))
              ((char= ch #\)) (read-ch-token state types:*close-paren* ")"))
              ((char= ch #\") (read-string-token state))
              ((char= ch #\;) (read-comment-token state))
              ((char= ch #\:) (read-colons-token state))
              ((is-ws ch) (read-ws-token state))
              (T (read-symbol-token state)))))


(defun from-stream (input)
    (loop :with state := (make-instance 'parse-state :input input)
          :while (look-ahead state)
          :collect (next-token state)))
