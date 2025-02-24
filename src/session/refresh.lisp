(defpackage :alive/session/refresh
    (:use :cl)
    (:export :new-send)
    (:local-nicknames (:deps :alive/deps)
                      (:logger :alive/logger)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:spawn :alive/session/spawn)))

(in-package :alive/session/refresh)


(declaim (ftype (function (deps:dependencies state:state) null) new-send))
(defun new-send (deps state)
    (spawn:new-thread "Refresh Thread"
        (let* ((send-id (state:new-next-send-id state))
               (response (deps:new-send-request deps (lsp-msg:create-request send-id "workspace/semanticTokens/refresh"))))
            (when (assoc :error response)
                  (logger:error-msg "Failed to refresh tokens: ~A" (cdr (assoc :error response))))
            (deps:new-send-msg deps (notification:refresh)))))
