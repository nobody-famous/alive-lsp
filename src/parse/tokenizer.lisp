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
     (start :accessor start
            :initform nil
            :initarg :start)
     (buffer :accessor buffer
             :initform nil
             :initarg :buffer)
     (line :accessor line
           :initform 0
           :initarg :line)
     (col :accessor col
          :initform 0
          :initarg :col)))


(defun start-token (state)
    (setf (start state)
          (pos:create :line (line state) :col (col state)))
    (setf (buffer state)
          (make-string-output-stream)))


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
        (write-char ch (buffer state))
        ch))


(defun new-token (state token-type &optional text)
    (let ((end (pos:create :line (line state)
                           :col (col state))))

        (token:create :type-value token-type
                      :start (start state)
                      :end end
                      :text (if text
                                text
                                (get-output-stream-string (buffer state))))))


(defun read-text-token (&key state token-type predicate)
    (loop :for ch := (look-ahead state)

          :while (and ch (funcall predicate ch))
          :do (next-char state)

          :finally (return (new-token state token-type))))


(defun read-ws-token (state)
    (read-text-token :state state
                     :token-type types:*ws*
                     :predicate (lambda (ch)
                                    (and ch (is-ws ch)))))


(defun read-symbol-token (state)
    (read-text-token :state state
                     :token-type types:*symbol*
                     :predicate (lambda (ch)
                                    (not (or (is-delim ch)
                                             (char= ch #\:))))))


(defun read-ch-token (state token-type)
    (next-char state)
    (new-token state token-type))


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
                  (new-token state types:*string*)))))


(defun read-comment-token (state)
    (read-text-token :state state
                     :token-type types:*comment*
                     :predicate (lambda (ch)
                                    (and ch
                                         (not (char= ch #\newline))))))


(defun read-colons-token (state)
    (read-text-token :state state
                     :token-type types:*colons*
                     :predicate (lambda (ch)
                                    (char= ch #\:))))


(defun read-ifdef-token (state sign-ch)
    (next-char state)

    (labels ((check-ifdef (text)
                  (let ((to-check (format nil "~A T" text)))
                      (ignore-errors (read-from-string to-check)))))

        (loop :with str := (make-string-output-stream)
              :with depth := 0
              :for ch := (look-ahead state)

              :until (and (eq 0 depth)
                          (or (not ch)
                              (is-ws ch)))
              :do (cond ((char= ch #\() (incf depth))
                        ((char= ch #\)) (when (< 0 depth)
                                              (decf depth))))
                  (next-char state)
                  (write-char ch str)

              :finally (return (let ((text (format nil "#~A~A" sign-ch (get-output-stream-string str))))
                                   (if (check-ifdef text)
                                       (new-token state types:*ifdef-true* text)
                                       (new-token state types:*ifdef-false* text)))))))


(defun read-block-comment-token (state)
    (next-char state)

    (loop :with depth := 0
          :with done := nil
          :with have-bar := nil
          :with have-pound := nil

          :for ch := (look-ahead state)
          :until done

          :do (cond ((char= ch #\|) (if have-pound
                                        (progn (incf depth)
                                               (setf have-pound nil))
                                        (setf have-bar t)))
                    ((char= ch #\#) (if have-bar
                                        (if (eq 0 depth)
                                            (setf done t)
                                            (progn (decf depth)
                                                   (setf have-bar nil)))
                                        (setf have-pound t))))
              (next-char state)

          :finally (return (new-token state types:*comment*))))


(defun read-macro-generic-token (state)
    (read-text-token :state state
                     :token-type types:*macro*
                     :predicate (lambda (ch)
                                    (and ch
                                         (not (is-delim ch))))))


(defun read-macro-token (state)
    (next-char state)

    (let ((ch (look-ahead state)))
        (cond ((or (char= ch #\+)
                   (char= ch #\-)) (read-ifdef-token state ch))
              ((char= ch #\|) (read-block-comment-token state))
              (t (read-macro-generic-token state)))))


(defun next-token (state)
    (start-token state)

    (let ((ch (look-ahead state)))
        (cond ((char= ch #\() (read-ch-token state types:*open-paren*))
              ((char= ch #\)) (read-ch-token state types:*close-paren*))
              ((char= ch #\") (read-string-token state))
              ((char= ch #\;) (read-comment-token state))
              ((char= ch #\:) (read-colons-token state))
              ((char= ch #\#) (read-macro-token state))
              ((is-ws ch) (read-ws-token state))
              (T (read-symbol-token state)))))


(defun from-stream (input)
    (loop :with state := (make-instance 'parse-state :input input)
          :while (look-ahead state)
          :collect (next-token state)))
