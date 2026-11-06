(defpackage :alive/session/handler/asdf
    (:use :cl)
    (:export :list-all
             :load-system)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/asdf)


(defun list-all (deps msg)
    (utils:result (cdr (assoc :id msg))
                  "systems"
                  (deps:list-all-asdf deps)))


(defun load-system (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (name (cdr (assoc :name params))))
        (when name
              (deps:load-asdf-system deps
                                     :name name
                                     :stdin-fn (lambda ()
                                                   (threads:wait-for-input deps state))
                                     :stdout-fn (lambda (data)
                                                    (deps:send-msg deps (notification:stdout data)))
                                     :stderr-fn (lambda (data)
                                                    (deps:send-msg deps (notification:stderr data)))))
        (deps:send-msg deps (lsp-msg:create-response id :result-value T))))
