(defpackage :alive/session/handler/compile
    (:use :cl)
    (:export :file
             :load-file
             :try)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:notification :alive/lsp/message/notification)
                      (:spawn :alive/session/spawn)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/compile)


(defun try (deps msg)
    (let* ((params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params)))
           (msgs (handler-case
                         (deps:try-compile deps path)
                     (T () nil))))
        (deps:send-msg deps (utils:result (cdr (assoc :id msg)) "messages" msgs))))


(defun file (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params))))

        (deps:do-compile deps path
                         :stdin-fn (lambda ()
                                       (threads:wait-for-input deps state))
                         :stdout-fn (lambda (data)
                                        (deps:send-msg deps (notification:stdout data)))
                         :stderr-fn (lambda (data)
                                        (deps:send-msg deps (notification:stderr data))))

        (deps:send-msg deps (lsp-msg:create-response id :result-value "OK"))))


(defun load-file (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params)))
           (msgs (deps:do-load deps path
                               :stdin-fn (lambda ()
                                             (threads:wait-for-input deps state))
                               :stdout-fn (lambda (data)
                                              (when (assoc :show-stdout params)
                                                    (deps:send-msg deps (notification:stdout data))))
                               :stderr-fn (lambda (data)
                                              (when (assoc :show-stderr params)
                                                    (deps:send-msg deps (notification:stderr data)))))))

        (deps:send-msg deps (utils:result id "messages" msgs))))
