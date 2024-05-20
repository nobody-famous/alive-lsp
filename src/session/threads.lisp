(defpackage :alive/session/threads
    (:use :cl)
    (:export :run-in-thread
             :wait-for-input)
    (:local-nicknames (:debugger :alive/debugger)
                      (:deps :alive/deps)
                      (:file-utils :alive/file-utils)
                      (:logger :alive/logger)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:req :alive/lsp/message/request)
                      (:restart-info :alive/lsp/types/restart-info)
                      (:state :alive/session/state)
                      (:thread-utils :alive/thread-utils)))

(in-package :alive/session/threads)


(declaim (ftype (function () string) wait-for-input))
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


(declaim (ftype (function ((or null string)) (or null stream)) get-frame-text-stream))
(defun get-frame-text-stream (file)
    (let* ((file-url (format NIL "file://~A" (file-utils:escape-file file)))
           (text (state:get-file-text file-url)))

        (when text
              (make-string-input-stream text))))


(declaim (ftype (function (hash-table) hash-table) frame-to-wire))
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


(declaim (ftype (function (condition cons cons)) wait-for-debug))
(defun wait-for-debug (err restarts frames)
    (let ((send-id (state:next-send-id))
          (cond-var (bt:make-condition-variable))
          (restart-ndx nil)
          (received nil))

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
                    (state:lock (mutex)
                        (setf received T)
                        (bt:condition-notify cond-var)))))

        (deps:send-msg (req:debugger send-id
                                     :message (princ-to-string err)
                                     :restarts restarts
                                     :stack-trace (mapcar #'frame-to-wire frames)))

        (state:lock (mutex)
            (unless received
                (bt:condition-wait cond-var mutex)))

        restart-ndx))


(declaim (ftype (function (condition cons) null) start-debugger))
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
              (invoke-restart-interactively (elt restarts ndx)))
        nil))


(declaim (ftype (function (function)) run-with-debugger))
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


(declaim (ftype (function (string) string) next-thread-name))
(defun next-thread-name (method-name)
    (format nil "~A - ~A" (state:next-thread-id) method-name))


(declaim (ftype (function (string cons function) bt:thread) run-in-thread))
(defun run-in-thread (method-name msg fn)
    (thread-utils:spawn-thread (next-thread-name method-name)
        (state:with-thread-msg ((cdr (assoc :id msg)))
            (unwind-protect
                    (run-with-debugger fn)
                (state:rem-thread-msg)))))
