(defpackage :alive/session/handler/compile
    (:use :cl)
    (:export :file
             :load-file
             :new-try
             :try)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:notification :alive/lsp/message/notification)
                      (:spawn :alive/session/spawn)
                      (:threads :alive/session/threads)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/compile)


(declaim (ftype (function (list) null) try))
(defun try (msg)
    (let* ((params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params)))
           (msgs (handler-case
                         (deps:try-compile path)
                     (T () nil))))
        (deps:send-msg (utils:result (cdr (assoc :id msg)) "messages" msgs))))


(declaim (ftype (function (deps:dependencies list) null) new-try))
(defun new-try (deps msg)
    (let* ((params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params)))
           (msgs (handler-case
                         (deps:new-try-compile deps path)
                     (T () nil))))
        (deps:new-send-msg deps (utils:result (cdr (assoc :id msg)) "messages" msgs))))


(declaim (ftype (function (list) null) try))
(defun file (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params))))

        (deps:do-compile path
                         :stdin-fn (lambda ()
                                       (threads:wait-for-input))
                         :stdout-fn (lambda (data)
                                        (deps:send-msg (notification:stdout data)))
                         :stderr-fn (lambda (data)
                                        (deps:send-msg (notification:stderr data))))

        (deps:send-msg (lsp-msg:create-response id :result-value "OK"))))


(declaim (ftype (function (list) null) load-file))
(defun load-file (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params)))
           (msgs (deps:do-load path
                               :stdin-fn (lambda ()
                                             (threads:wait-for-input))
                               :stdout-fn (lambda (data)
                                              (when (assoc :show-stdout params)
                                                    (deps:send-msg (notification:stdout data))))
                               :stderr-fn (lambda (data)
                                              (when (assoc :show-stderr params)
                                                    (deps:send-msg (notification:stderr data)))))))

        (deps:send-msg (utils:result id "messages" msgs))))
