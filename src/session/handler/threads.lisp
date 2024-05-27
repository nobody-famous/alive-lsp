(defpackage :alive/session/handler/threads
    (:use :cl)
    (:export :kill
             :list-all)
    (:local-nicknames (:deps :alive/deps)
                      (:errors :alive/lsp/errors)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:refresh :alive/session/refresh)
                      (:state :alive/session/state)
                      (:threads :alive/threads)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/threads)


(declaim (ftype (function (cons) hash-table) list-all))
(defun list-all (msg)
    (state:lock (mutex)
        (let ((threads (remove-if (lambda (thread)
                                      (eq (cdr (assoc :id thread)) (deps:get-thread-id (bt:current-thread))))
                               (deps:list-all-threads))))

            (utils:result (cdr (assoc :id msg)) "threads" threads))))


(declaim (ftype (function (T)) cancel-thread))
(defun cancel-thread (thread-id)
    (let ((msg-id (state:get-thread-msg thread-id)))
        (when msg-id
              (deps:send-msg (lsp-msg:create-error msg-id
                                                   :code errors:*request-cancelled*
                                                   :message (format nil "Request ~A canceled" msg-id))))

        (when thread-id
              (deps:kill-thread thread-id))))


(declaim (ftype (function (cons) (values hash-table &optional)) kill))
(defun kill (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (thread-id (cdr (assoc :id params))))

        (handler-case
                (progn (cancel-thread thread-id)
                       (refresh:send)
                       (lsp-msg:create-response id :result-value T))

            (threads:thread-not-found (c)
                                      (lsp-msg:create-error id
                                                            :code errors:*request-failed*
                                                            :message (format nil "Thread ~A not found" (threads:id c)))))))
