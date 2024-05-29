(defpackage :alive/session/handler/asdf
    (:use :cl)
    (:export :load-asdf-event
             :list-all
             :load-system)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/asdf)


(define-condition load-asdf-event ()
        ((thread :accessor thread
                 :initform nil
                 :initarg :thread))
    (:report (lambda (condition stream) (format stream "THREAD ~A" (thread condition)))))


(declaim (ftype (function (cons) hash-table) list-all))
(defun list-all (msg)
    (utils:result (cdr (assoc :id msg))
                  "systems"
                  (deps:list-all-asdf)))


(declaim (ftype (function (cons) (values null &optional)) load-system))
(defun load-system (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (name (cdr (assoc :name params)))
           (thread (threads:run-in-thread (or (cdr (assoc :method msg)) "ASDF")
                                          msg
                                          (lambda ()
                                              (deps:load-asdf-system
                                                  :name name
                                                  :stdin-fn (lambda ()
                                                                (threads:wait-for-input))
                                                  :stdout-fn (lambda (data)
                                                                 (deps:send-msg (notification:stdout data)))
                                                  :stderr-fn (lambda (data)
                                                                 (deps:send-msg (notification:stderr data))))
                                              (deps:send-msg (lsp-msg:create-response id :result-value T))))))
        (signal (make-condition 'load-asdf-event :thread thread))))
