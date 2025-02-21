(defpackage :alive/session/handler/asdf
    (:use :cl)
    (:export :list-all
             :load-system
             :new-list-all
             :new-load-system)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/asdf)


(declaim (ftype (function (cons) hash-table) list-all))
(defun list-all (msg)
    (utils:result (cdr (assoc :id msg))
                  "systems"
                  (deps:list-all-asdf)))


(declaim (ftype (function (deps:dependencies cons) hash-table) new-list-all))
(defun new-list-all (deps msg)
    (utils:result (cdr (assoc :id msg))
                  "systems"
                  (deps:new-list-all-asdf deps)))


(declaim (ftype (function (cons) (values null &optional)) load-system))
(defun load-system (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (name (cdr (assoc :name params))))
        (deps:load-asdf-system
            :name name
            :stdin-fn (lambda ()
                          (threads:wait-for-input))
            :stdout-fn (lambda (data)
                           (deps:send-msg (notification:stdout data)))
            :stderr-fn (lambda (data)
                           (deps:send-msg (notification:stderr data))))
        (deps:send-msg (lsp-msg:create-response id :result-value T))))


(declaim (ftype (function (deps:dependencies cons) (values null &optional)) new-load-system))
(defun new-load-system (deps msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (name (cdr (assoc :name params))))
        (deps:new-load-asdf-system deps
                                   :name name
                                   :stdin-fn (lambda ()
                                                 (threads:new-wait-for-input deps))
                                   :stdout-fn (lambda (data)
                                                  (deps:new-send-msg deps (notification:stdout data)))
                                   :stderr-fn (lambda (data)
                                                  (deps:new-send-msg deps (notification:stderr data))))
        (deps:new-send-msg deps (lsp-msg:create-response id :result-value T))))
