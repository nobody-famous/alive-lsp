(defpackage :alive/session/handler/compile
    (:use :cl)
    (:export :new-file
             :new-load-file
             :new-try)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:notification :alive/lsp/message/notification)
                      (:spawn :alive/session/spawn)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/compile)


(declaim (ftype (function (deps:dependencies list) null) new-try))
(defun new-try (deps msg)
    (let* ((params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params)))
           (msgs (handler-case
                         (deps:new-try-compile deps path)
                     (T () nil))))
        (deps:new-send-msg deps (utils:result (cdr (assoc :id msg)) "messages" msgs))))


(declaim (ftype (function (deps:dependencies state:state list) null) new-file))
(defun new-file (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params))))

        (deps:new-do-compile deps path
                             :stdin-fn (lambda ()
                                           (threads:new-wait-for-input deps state))
                             :stdout-fn (lambda (data)
                                            (deps:new-send-msg deps (notification:stdout data)))
                             :stderr-fn (lambda (data)
                                            (deps:new-send-msg deps (notification:stderr data))))

        (deps:new-send-msg deps (lsp-msg:create-response id :result-value "OK"))))


(declaim (ftype (function (deps:dependencies state:state list) null) new-load-file))
(defun new-load-file (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params)))
           (msgs (deps:new-do-load deps path
                                   :stdin-fn (lambda ()
                                                 (threads:new-wait-for-input deps state))
                                   :stdout-fn (lambda (data)
                                                  (when (assoc :show-stdout params)
                                                        (deps:new-send-msg deps (notification:stdout data))))
                                   :stderr-fn (lambda (data)
                                                  (when (assoc :show-stderr params)
                                                        (deps:new-send-msg deps (notification:stderr data)))))))

        (deps:new-send-msg deps (utils:result id "messages" msgs))))
