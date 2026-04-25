(defpackage :alive/session/handler/eval
    (:use :cl)
    (:export :handle
             :handle-in-frame)
    (:local-nicknames (:deps :alive/deps)
                      (:eval :alive/sys/eval)
                      (:debugger :alive/debugger)
                      (:handler-utils :alive/session/handler/utils)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)))

(in-package :alive/session/handler/eval)


(declaim (ftype (function (deps:dependencies state:state cons) null) handle))
(defun handle (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (or (cdr (assoc :package params)) "cl-user"))
           (text (cdr (assoc :text params)))
           (* (state:get-history-item state 0))
           (** (state:get-history-item state 1))
           (*** (state:get-history-item state 2))
           (results (eval:from-string deps text
                                      :pkg-name pkg-name
                                      :stdin-fn (lambda ()
                                                    (threads:wait-for-input deps state))
                                      :stdout-fn (lambda (data)
                                                     (deps:send-msg deps (notification:stdout data)))
                                      :query-fn (lambda (data)
                                                    (deps:send-msg deps (notification:query data)))
                                      :trace-fn (lambda (data)
                                                    (deps:send-msg deps (notification:stdout data)))
                                      :stderr-fn (lambda (data)
                                                     (deps:send-msg deps (notification:stderr data))))))

        (when (cdr (assoc :store-result params))
              (state:add-history state (car results)))

        (let ((response-content (if (cdr results)
                                    (mapcar (lambda (r) (format nil "~A" r)) results)
                                    (format nil "~A" (car results)))))
            (deps:send-msg deps (handler-utils:result id "text" response-content)))))


(declaim (ftype (function (state:state cons) null) handle-in-frame))
(defun handle-in-frame (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (debugger-id (cdr (assoc :id params)))
           (frame (cdr (assoc :frame params)))
           (text (cdr (assoc :text params)))
           (frames (state:get-debugger state debugger-id)))

        (format T "HANDLE-IN-FRAME ~A~%" msg)
        (when (and frames
                   (numberp frame)
                   (stringp text))
              (debugger:eval-in-frame (nth frame frames) text))))
