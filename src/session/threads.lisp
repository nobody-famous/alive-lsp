(defpackage :alive/session/threads
    (:use :cl)
    (:export :xyz-run-in-thread
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


(defun wait-for-input (deps state)
    (let ((input-resp (deps:send-request deps (lsp-msg:create-request (state:next-send-id state) "$/alive/userInput"))))

        (cond ((assoc :error input-resp)
                  (logger:error-msg (state:get-log state) "Input Error ~A" input-resp))

              ((assoc :result input-resp)
                  (let* ((result (cdr (assoc :result input-resp))))
                      (or (cdr (assoc :text result))
                          ""))))))


(defun get-frame-text-stream (state file)
    (when file
          (let* ((file-url (format NIL "file://~A" (file-utils:escape-file file)))
                 (text (state:get-file-text state file-url)))

              (when text
                    (make-string-input-stream text)))))


(defun stringify-vars (vars)
    (loop :for var :in vars
          :do (setf (gethash "value" var) (alive/utils:safe-print (gethash"value"var))))
    vars)


(defun xyz-frame-to-wire (state frame)
    (let* ((obj (make-hash-table :test #'equalp))
           (file (gethash "file" frame))
           (vars (gethash "vars" frame))
           (restartable (if (gethash "restartable" frame) T nil))
           (args-list (gethash "argsList" frame))
           (fn-name (gethash "function" frame))
           (pos (debugger:xyz-get-frame-loc (get-frame-text-stream state file)
                                            frame)))

        (setf (gethash "function" obj) fn-name)
        (setf (gethash "file" obj) file)
        (setf (gethash "position" obj) pos)
        (setf (gethash "restartable" obj) restartable)
        (setf (gethash "argsList" obj) args-list)
        (setf (gethash "vars" obj) (if (consp vars)
                                       (stringify-vars vars)
                                       vars))

        obj))


(defun xyz-wait-for-debug (deps state err restarts frames)
    (let* ((debugger-id (state:next-send-id state))
           (request (req:debugger debugger-id
                                  :debugger-id debugger-id
                                  :message (princ-to-string err)
                                  :restarts restarts
                                  :stack-trace (mapcar (lambda (frame)
                                                           (xyz-frame-to-wire state frame))
                                                       (mapcar (lambda (frame) (car frame)) frames)))))
        (state:set-debugger state debugger-id (mapcar (lambda (frame) (cdr frame)) frames))

        (let ((debug-resp (deps:send-request deps request)))
            (state:remove-debugger state debugger-id)

            (cond ((assoc :error debug-resp)
                      (logger:error-msg (state:get-log state) "Debugger Error ~A" debug-resp))

                  ((assoc :result debug-resp)
                      (cdr (assoc :result debug-resp)))))))


(defun do-restart (restarts ndx)
    (when (and ndx
               (<= 0 ndx (- (length restarts) 1)))
          (invoke-restart-interactively (elt restarts ndx))))


(defun do-restart-frame (frames ndx args-list)
    (let ((frame (cdr (nth ndx frames))))
        (when (sb-debug:frame-has-debug-tag-p frame)
              (multiple-value-bind (fname args) (sb-debug::frame-call frame)
                  (when (and (sb-int:legal-fun-name-p fname)
                             (fboundp fname))
                        (let ((fun (fdefinition fname))
                              (fun-args (if (< 0 (length args-list))
                                            (sb-di:eval-in-frame frame (read-from-string args-list))
                                            args)))
                            (sb-debug:unwind-to-frame-and-call
                                frame
                                (lambda ()
                                    (declare (optimize (debug 0)))
                                    (apply fun fun-args)))))))))


(defun xyz-start-debugger (deps state err frames)
    (let* ((restarts (compute-restarts err))
           (action (xyz-wait-for-debug deps state err
                                       (mapcar (lambda (item)
                                                   (restart-info:create-item :name (restart-name item)
                                                                             :description (princ-to-string item)))
                                               restarts)
                                       frames)))

        (cond ((assoc :restart action)
                  (do-restart restarts (cdr (assoc :restart action))))
              ((assoc :restart-frame action)
                  (do-restart-frame frames
                                    (cdr (assoc :restart-frame action))
                                    (cdr (assoc :restart-args-list action)))))
        nil))


(defun xyz-run-with-debugger (deps state fn)
    (let ((sb-ext:*invoke-debugger-hook* (lambda (c h)
                                             (declare (ignore h))
                                             (xyz-start-debugger deps state c (alive/frames:list-debug-frames))
                                             (return-from xyz-run-with-debugger)))
          (*debugger-hook* (lambda (c h)
                               (declare (ignore h))
                               (xyz-start-debugger deps state c (alive/frames:list-debug-frames))
                               (return-from xyz-run-with-debugger))))
        (funcall fn)))


(defun next-thread-name (state method-name)
    (format nil "~A - ~A" (state:next-thread-id state) method-name))


(defun xyz-run-in-thread (deps state method-name msg-id fn)
    (spawn:new-thread (next-thread-name state method-name)
        (state:with-thread-msg (state deps msg-id)
            (unwind-protect
                    (progn (refresh:send deps state)
                           (xyz-run-with-debugger deps state fn))
                (refresh:send deps state)))))
