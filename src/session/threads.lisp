(defpackage :alive/session/threads
    (:use :cl)
    (:export :run-in-thread
             :wait-for-input)
    (:local-nicknames (:debugger :alive/debugger)
                      (:deps :alive/deps)
                      (:file-utils :alive/file-utils)
                      (:logger :alive/logger)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:refresh :alive/session/refresh)
                      (:req :alive/lsp/message/request)
                      (:restart-info :alive/lsp/types/restart-info)
                      (:state :alive/session/state)
                      (:spawn :alive/session/spawn)))

(in-package :alive/session/threads)


(declaim (ftype (function () (values (or null string) &optional)) wait-for-input))
(defun wait-for-input ()
    (let ((input-resp (deps:send-request (lsp-msg:create-request (state:next-send-id) "$/alive/userInput"))))

        (cond ((assoc :error input-resp)
                  (logger:error-msg "Input Error ~A" input-resp))

              ((assoc :result input-resp)
                  (let* ((result (cdr (assoc :result input-resp))))
                      (or (cdr (assoc :text result))
                          ""))))))


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
    (let ((debug-resp (deps:send-request (req:debugger (state:next-send-id)
                                                       :message (princ-to-string err)
                                                       :restarts restarts
                                                       :stack-trace (mapcar #'frame-to-wire frames)))))

        (cond ((assoc :error debug-resp)
                  (logger:error-msg "Debugger Error ~A" debug-resp))

              ((assoc :result debug-resp)
                  (let* ((result (cdr (assoc :result debug-resp))))
                      (cdr (assoc :index result)))))))


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


(declaim (ftype (function (string cons function) null) run-in-thread))
(defun run-in-thread (method-name msg fn)
    (spawn:new-thread (next-thread-name method-name)
        (state:with-thread-msg ((cdr (assoc :id msg)))
            (unwind-protect
                    (progn (refresh:send)
                           (run-with-debugger fn))
                (refresh:send)))))
