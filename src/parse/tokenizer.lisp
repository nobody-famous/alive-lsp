(defpackage :alive/parse/tokenizer
    (:use :cl)
    (:export :from-stream)
    (:local-nicknames (:pos :alive/position)
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
         (start-offset :accessor start-offset
                       :initform nil
                       :initarg :start-offset)
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
        (pos:create (line state) (col state)))
    (setf (start-offset state)
        (file-position (input state)))
    (setf (buffer state)
        (make-string-output-stream)))


(defun look-ahead (state)
    (peek-char nil (input state) nil nil t))


(defun is-ws (ch)
    (and ch
         (or (char= ch #\space)
             (not (graphic-char-p ch)))))


(defun is-delim (ch)
    (or (not ch)
        (is-ws ch)
        (char= ch #\))
        (char= ch #\()))


(defun is-macro-ch (ch)
    (and ch
         (not (is-ws ch))
         (not (char= #\( ch))
         (not (char= #\) ch))))


(defun next-char (state)
    (let ((ch (read-char (input state) nil nil)))
        (when ch
              (cond ((char= ch #\newline) (incf (line state))
                                          (setf (col state) 0))
                    (T (incf (col state))))
              (write-char ch (buffer state))
              ch)))


(defun new-token (state token-type &optional text)
    (let ((end (pos:create (line state) (col state))))

        (token:create :type-value token-type
                      :start (start state)
                      :start-offset (start-offset state)
                      :end end
                      :end-offset (file-position (input state))
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


(defun try-read (state)
    (handler-case
            (read-preserving-whitespace (input state))
        (error (c)
            (declare (ignore c))
            nil)))


(defun read-string-token (state)
    (let ((start (file-position (input state))))

        (try-read state)

        ; In case the string spans multiple lines, reset to the start
        ; and read character by character to the end so the state gets
        ; updated.

        (let ((end (file-position (input state))))
            (file-position (input state) start)
            (loop :until (eq end (file-position (input state)))
                  :do (next-char state))

            (new-token state types:*string*))))


(defun read-comment-token (state)
    (read-text-token :state state
                     :token-type types:*line-comment*
                     :predicate (lambda (ch)
                                    (and ch
                                         (not (char= ch #\return))
                                         (not (char= ch #\newline))))))


(defun read-colons-token (state)
    (read-text-token :state state
                     :token-type types:*colons*
                     :predicate (lambda (ch)
                                    (and ch
                                         (char= ch #\:)))))


(defun read-ifdef-token (state sign-ch)
    (next-char state)

    (labels ((check-ifdef (text)
                          (let ((to-check (format nil "~A T" text)))
                              (ignore-errors (read-from-string to-check)))))

        (loop :with str := (make-string-output-stream)
              :with depth := 0
              :for ch := (look-ahead state)

              :until (or (not ch)
                         (and (eq 0 depth)
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
          :until (or (not ch)
                     done)

          :do (cond ((char= ch #\|)
                        (if have-pound
                            (progn (incf depth)
                                   (setf have-pound nil))
                            (setf have-bar t)))

                    ((char= ch #\#)
                        (if have-bar
                            (if (eq 0 depth)
                                (setf done t)
                                (progn (decf depth)
                                       (setf have-bar nil)))
                            (setf have-pound t)))

                    (T (setf have-pound nil)
                       (setf have-bar nil)))

              (next-char state)

          :finally (return (new-token state types:*block-comment*))))


(defun read-macro-generic-token (state)
    (read-text-token :state state
                     :token-type types:*macro*
                     :predicate (lambda (ch)
                                    (and ch
                                         (not (is-delim ch))))))


(defun read-macro-char (state)
    (next-char state)

    (let ((ch (look-ahead state)))
        (if (is-macro-ch ch)
            (loop :while (is-macro-ch ch)
                  :do (next-char state)
                      (setf ch (look-ahead state)))
            (next-char state))

        (new-token state types:*macro*)))


(defun read-macro-token (state)
    (next-char state)

    (let ((ch (look-ahead state)))
        (if ch
            (cond ((or (char= ch #\+)
                       (char= ch #\-)) (read-ifdef-token state ch))
                  ((char= ch #\|) (read-block-comment-token state))
                  ((char= ch #\\) (read-macro-char state))
                  (t (read-macro-generic-token state)))
            (new-token state types:*macro*))))


(defun read-comma-token (state)
    (next-char state)

    (if (char= #\@ (look-ahead state))
        (progn (next-char state)
               (new-token state types:*comma-at*))
        (new-token state types:*comma*)))


(defun next-token (state)
    (start-token state)

    (let ((ch (look-ahead state)))
        (when ch
              (cond ((char= ch #\() (read-ch-token state types:*open-paren*))
                    ((char= ch #\)) (read-ch-token state types:*close-paren*))
                    ((char= ch #\') (read-ch-token state types:*quote*))
                    ((char= ch #\`) (read-ch-token state types:*back-quote*))
                    ((char= ch #\") (read-string-token state))
                    ((char= ch #\;) (read-comment-token state))
                    ((char= ch #\:) (read-colons-token state))
                    ((char= ch #\#) (read-macro-token state))
                    ((char= ch #\,) (read-comma-token state))
                    ((is-ws ch) (read-ws-token state))
                    (T (read-symbol-token state))))))


(defun from-stream (input)
    (loop :with state := (make-instance 'parse-state :input input)

          :while (look-ahead state)
          :collect (next-token state)))
