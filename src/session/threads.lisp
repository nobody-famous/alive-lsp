(defpackage :alive/session/threads
    (:use :cl)
    (:export :new-run-in-thread
             :new-wait-for-input)
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


(declaim (ftype (function (deps:dependencies state:state) (values (or null string) &optional)) new-wait-for-input))
(defun new-wait-for-input (deps state)
    (let ((input-resp (deps:send-request deps (lsp-msg:create-request (state:new-next-send-id state) "$/alive/userInput"))))

        (cond ((assoc :error input-resp)
                  (logger:error-msg "Input Error ~A" input-resp))

              ((assoc :result input-resp)
                  (let* ((result (cdr (assoc :result input-resp))))
                      (or (cdr (assoc :text result))
                          ""))))))


(declaim (ftype (function (state:state (or null string)) (or null stream)) new-get-frame-text-stream))
(defun new-get-frame-text-stream (state file)
    (when file
          (let* ((file-url (format NIL "file://~A" (file-utils:escape-file file)))
                 (text (state:new-get-file-text state file-url)))

              (when text
                    (make-string-input-stream text)))))


(declaim (ftype (function (state:state hash-table) hash-table) new-frame-to-wire))
(defun new-frame-to-wire (state frame)
    (let* ((obj (make-hash-table :test #'equalp))
           (file (gethash "file" frame))
           (fn-name (gethash "function" frame))
           (pos (debugger:get-frame-loc (new-get-frame-text-stream state file)
                                        frame)))

        (setf (gethash "function" obj) fn-name)
        (setf (gethash "file" obj) file)
        (setf (gethash "position" obj) pos)

        obj))


(declaim (ftype (function (deps:dependencies state:state condition cons cons)) new-wait-for-debug))
(defun new-wait-for-debug (deps state err restarts frames)
    (let ((debug-resp (deps:send-request deps (req:debugger (state:new-next-send-id state)
                                                                :message (princ-to-string err)
                                                                :restarts restarts
                                                                :stack-trace (mapcar (lambda (frame) (new-frame-to-wire state frame)) frames)))))

        (cond ((assoc :error debug-resp)
                  (logger:error-msg "Debugger Error ~A" debug-resp))

              ((assoc :result debug-resp)
                  (let* ((result (cdr (assoc :result debug-resp))))
                      (cdr (assoc :index result)))))))


(declaim (ftype (function (deps:dependencies state:state condition cons) null) new-start-debugger))
(defun new-start-debugger (deps state err frames)
    (let* ((restarts (compute-restarts err))
           (ndx (new-wait-for-debug deps state err
                                    (mapcar (lambda (item)
                                                (restart-info:create-item :name (restart-name item)
                                                                          :description (princ-to-string item)))
                                            restarts)
                                    frames)))

        (when (and ndx
                   (<= 0 ndx (- (length restarts) 1)))
              (invoke-restart-interactively (elt restarts ndx)))
        nil))


(declaim (ftype (function (deps:dependencies state:state function)) new-run-with-debugger))
(defun new-run-with-debugger (deps state fn)
    (let ((sb-ext:*invoke-debugger-hook* (lambda (c h)
                                             (declare (ignore h))
                                             (new-start-debugger deps state c (alive/frames:list-debug-frames))
                                             (return-from new-run-with-debugger)))
          (*debugger-hook* (lambda (c h)
                               (declare (ignore h))
                               (new-start-debugger deps state c (alive/frames:list-debug-frames))
                               (return-from new-run-with-debugger))))
        (funcall fn)))


(declaim (ftype (function (state:state string) string) new-next-thread-name))
(defun new-next-thread-name (state method-name)
    (format nil "~A - ~A" (state:new-next-thread-id state) method-name))


(declaim (ftype (function (deps:dependencies state:state string integer function) null) new-run-in-thread))
(defun new-run-in-thread (deps state method-name msg-id fn)
    (spawn:new-thread (new-next-thread-name state method-name)
        (state:new-with-thread-msg (state deps msg-id)
            (unwind-protect
                    (progn (refresh:new-send deps state)
                           (new-run-with-debugger deps state fn))
                (refresh:new-send deps state)))))
