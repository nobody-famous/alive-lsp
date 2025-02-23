(defpackage :alive/session/handler/threads
    (:use :cl)
    (:export :kill
             :list-all
             :new-cancel-thread
             :new-kill
             :new-list-all)
    (:local-nicknames (:deps :alive/deps)
                      (:errors :alive/lsp/errors)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:refresh :alive/session/refresh)
                      (:state :alive/session/state)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/threads)


(declaim (ftype (function (cons) hash-table) list-all))
(defun list-all (msg)
    (state:lock (mutex)
        (let ((threads (remove-if (lambda (thread)
                                      (eq (cdr (assoc :id thread)) (deps:get-thread-id (bt:current-thread))))
                               (deps:list-all-threads))))

            (utils:result (cdr (assoc :id msg)) "threads" threads))))


(declaim (ftype (function (deps:dependencies state:state cons) hash-table) new-list-all))
(defun new-list-all (deps state msg)
    (state:new-lock (state mutex)
        (let ((threads (remove-if (lambda (thread)
                                      (eq (cdr (assoc :id thread)) (deps:new-get-thread-id deps (bt:current-thread))))
                               (deps:new-list-all-threads deps))))

            (utils:result (cdr (assoc :id msg)) "threads" threads))))


(declaim (ftype (function (T)) cancel-thread))
(defun cancel-thread (thread-id)
    (let ((msg-id (state:get-thread-msg thread-id)))
        (when msg-id
              (deps:send-msg (lsp-msg:create-error msg-id
                                                   :code errors:*request-cancelled*
                                                   :message (format nil "Request ~A canceled" msg-id))))

        (when thread-id
              (ignore-errors (deps:kill-thread thread-id)))))


(declaim (ftype (function (deps:dependencies state:state T)) new-cancel-thread))
(defun new-cancel-thread (deps state thread-id)
    (let ((msg-id (state:new-get-thread-msg state thread-id)))
        (when msg-id
              (deps:new-send-msg deps (lsp-msg:create-error msg-id
                                                            :code errors:*request-cancelled*
                                                            :message (format nil "Request ~A canceled" msg-id))))

        (when thread-id
              (ignore-errors (deps:new-kill-thread deps thread-id)))))


(declaim (ftype (function (cons) (values hash-table &optional)) kill))
(defun kill (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (thread-id (cdr (assoc :id params))))

        (cancel-thread thread-id)
        (refresh:send)
        (lsp-msg:create-response id :result-value T)))


(declaim (ftype (function (deps:dependencies state:state cons) (values hash-table &optional)) new-kill))
(defun new-kill (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (thread-id (cdr (assoc :id params))))

        (new-cancel-thread deps state thread-id)
        (refresh:new-send deps state)
        (lsp-msg:create-response id :result-value T)))
