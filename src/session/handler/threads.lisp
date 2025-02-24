(defpackage :alive/session/handler/threads
    (:use :cl)
    (:export :new-cancel-thread
             :new-kill
             :new-list-all)
    (:local-nicknames (:deps :alive/deps)
                      (:errors :alive/lsp/errors)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:refresh :alive/session/refresh)
                      (:state :alive/session/state)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/threads)


(declaim (ftype (function (deps:dependencies state:state cons) hash-table) new-list-all))
(defun new-list-all (deps state msg)
    (state:new-lock (state mutex)
        (let ((threads (remove-if (lambda (thread)
                                      (eq (cdr (assoc :id thread)) (deps:get-thread-id deps (bt:current-thread))))
                               (deps:list-all-threads deps))))

            (utils:result (cdr (assoc :id msg)) "threads" threads))))


(declaim (ftype (function (deps:dependencies state:state T)) new-cancel-thread))
(defun new-cancel-thread (deps state thread-id)
    (let ((msg-id (state:new-get-thread-msg state thread-id)))
        (when msg-id
              (deps:send-msg deps (lsp-msg:create-error msg-id
                                                            :code errors:*request-cancelled*
                                                            :message (format nil "Request ~A canceled" msg-id))))

        (when thread-id
              (ignore-errors (deps:kill-thread deps thread-id)))))


(declaim (ftype (function (deps:dependencies state:state cons) (values hash-table &optional)) new-kill))
(defun new-kill (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (thread-id (cdr (assoc :id params))))

        (new-cancel-thread deps state thread-id)
        (refresh:new-send deps state)
        (lsp-msg:create-response id :result-value T)))
