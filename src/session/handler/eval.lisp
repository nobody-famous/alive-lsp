(defpackage :alive/session/handler/eval
    (:use :cl)
    (:export :new-handle)
    (:local-nicknames (:deps :alive/deps)
                      (:eval :alive/sys/eval)
                      (:handler-utils :alive/session/handler/utils)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)))

(in-package :alive/session/handler/eval)


(declaim (ftype (function (deps:dependencies state:state cons) null) new-handle))
(defun new-handle (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (text (cdr (assoc :text params)))
           (* (state:get-history-item state 0))
           (** (state:get-history-item state 1))
           (*** (state:get-history-item state 2))
           (results (state:lock (state mutex)
                        (eval:new-from-string deps text
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
                                                             (deps:send-msg deps (notification:stderr data)))))))

        (when (cdr (assoc :store-result params))
              (state:add-history state results))

        (let ((response-content (if (= (length results) 1)
                                    (format nil "~A" (car results))
                                    (mapcar (lambda (r) (format nil "~A" r)) results))))
            (deps:send-msg deps (handler-utils:result id "text" response-content)))))
