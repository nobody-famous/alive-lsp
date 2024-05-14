(defpackage :alive/session/utils
    (:use :cl)
    (:export :run-in-thread
             :spawn-thread
             :wait-for-input)
    (:local-nicknames (:debugger :alive/debugger)
                      (:deps :alive/session/deps)
                      (:logger :alive/logger)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:req :alive/lsp/message/request)
                      (:restart-info :alive/lsp/types/restart-info)
                      (:state :alive/session/state)))

(in-package :alive/session/utils)


(defmacro spawn-thread (name &body body)
    (let ((stdin (gensym))
          (stdout (gensym))
          (logger (gensym))
          (state (gensym))
          (context (gensym))
          (handlers (gensym))
          (deps (gensym)))

        `(let ((,stdout *standard-output*)
               (,stdin *standard-input*)
               (,logger alive/logger:*logger*)
               (,state alive/session/state::*state*)
               (,context alive/context::*context*)
               (,handlers alive/session/handlers::*handlers*)
               (,deps alive/session/deps::*deps*))
             (bt:make-thread (lambda ()
                                 (let ((*standard-output* ,stdout)
                                       (*standard-input* ,stdin)
                                       (alive/logger:*logger* ,logger)
                                       (alive/session/state::*state* ,state)
                                       (alive/context::*context* ,context)
                                       (alive/session/handlers::*handlers* ,handlers)
                                       (alive/session/deps::*deps* ,deps))
                                     (progn ,@body)))
                             :name ,name))))


(declaim (ftype (function (string) string) next-thread-name))
(defun next-thread-name (method-name)
    (format nil "~A - ~A" (state:next-thread-id) method-name))


(defun is-win-file (file)
    (and file
         (alpha-char-p (char file 0))
         (char= #\: (char file 1))
         (char= #\/ (char file 2))))


(defun escape-win-file (file)
    (format nil "~C%3A~A"
        (char file 0)
        (subseq file 2)))


(defun escape-file (file)
    (if (is-win-file file)
        (format nil "/~A" (escape-win-file file))
        file))


(defun get-frame-text-stream (file)
    (let* ((file-url (format NIL "file://~A" (escape-file file)))
           (files (state:get-files))
           (text (gethash file-url files)))

        (when text
              (make-string-input-stream text))))


(defun frame-to-wire (frame)
    (let* ((obj (make-hash-table :test #'equalp))
           (file (gethash "file" frame))
           (fn-name (gethash "function" frame))
           (pos (debugger:get-frame-loc (get-frame-text-stream file)
                                        frame)))

        (setf (gethash "function" obj) fn-name)
        (setf (gethash "file" obj) file)
        (setf (gethash "position" obj) pos)

        obj))


(defun wait-for-input ()
    (let ((send-id (state:next-send-id))
          (cond-var (bt:make-condition-variable))
          (text nil))

        (state:set-sent-msg-callback
            send-id
            (lambda (input-resp)
                (unwind-protect
                        (cond ((assoc :error input-resp)
                                  (logger:error-msg "Input Error ~A" input-resp))

                              ((assoc :result input-resp)
                                  (let* ((result (cdr (assoc :result input-resp)))
                                         (text-result (if result
                                                          (cdr (assoc :text result))
                                                          "")))
                                      (setf text text-result))))
                    (bt:condition-notify cond-var))))

        (deps:send-msg (lsp-msg:create-request send-id "$/alive/userInput"))

        (state:lock (mutex)
            (bt:condition-wait cond-var mutex))

        text))


(defun wait-for-debug (err restarts frames)
    (let ((send-id (state:next-send-id))
          (cond-var (bt:make-condition-variable))
          (restart-ndx nil))

        (state:set-sent-msg-callback
            send-id
            (lambda (debug-resp)
                (unwind-protect
                        (cond ((assoc :error debug-resp)
                                  (logger:error-msg "Debugger Error ~A" debug-resp))

                              ((assoc :result debug-resp)
                                  (let* ((result (cdr (assoc :result debug-resp))))
                                      (when result
                                            (setf restart-ndx (cdr (assoc :index result)))))))
                    (bt:condition-notify cond-var))))

        (deps:send-msg (req:debugger send-id
                                     :message (princ-to-string err)
                                     :restarts restarts
                                     :stack-trace (mapcar (lambda (frame)
                                                              (frame-to-wire frame))
                                                          frames)))

        (state:lock (mutex)
            (bt:condition-wait cond-var mutex))

        restart-ndx))


(defun start-debugger (err frames)
    (let* ((restarts (compute-restarts err))
           (ndx (wait-for-debug err
                                (mapcar (lambda (item)
                                            (restart-info:create-item :name (restart-name item)
                                                                      :description (princ-to-string item)))
                                        restarts)
                                frames)))

        (when (and ndx
                   (<= 0 ndx (- (length restarts) 1)))
              (invoke-restart-interactively (elt restarts ndx)))))


(defun run-with-debugger (fn)
    (let ((sb-ext:*invoke-debugger-hook* (lambda (c h)
                                             (declare (ignore h))
                                             (start-debugger c (alive/frames:list-debug-frames))
                                             (return-from run-with-debugger)))
          (*debugger-hook* (lambda (c h)
                               (declare (ignore h))
                               (start-debugger c (alive/frames:list-debug-frames))
                               (return-from run-with-debugger))))
        (funcall fn)))


(defun run-in-thread (method-name msg fn)
    (spawn-thread (next-thread-name method-name)
        (state:with-thread-msg ((cdr (assoc :id msg)))
            (unwind-protect
                    (run-with-debugger fn)
                (state:rem-thread-msg)))))
