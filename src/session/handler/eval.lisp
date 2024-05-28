(defpackage :alive/session/handler/eval
    (:use :cl)
    (:export :eval-thread-event
             :thread
             :handle)
    (:local-nicknames (:deps :alive/deps)
                      (:eval :alive/eval)
                      (:handler-utils :alive/session/handler/utils)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)))

(in-package :alive/session/handler/eval)


(define-condition eval-thread-event ()
        ((thread :accessor thread
                 :initform nil
                 :initarg :thread))
    (:report (lambda (condition stream) (format stream "THREAD ~A" (thread condition)))))


(declaim (ftype (function (cons) null) process-eval))
(defun process-eval (msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (text (cdr (assoc :text params)))
           (* (state:get-history-item 0))
           (** (state:get-history-item 1))
           (*** (state:get-history-item 2))
           (result (eval::new-from-string text
                                          :pkg-name pkg-name
                                          :stdin-fn (lambda ()
                                                        (threads:wait-for-input))
                                          :stdout-fn (lambda (data)
                                                         (deps:send-msg (notification:stdout data)))
                                          :trace-fn (lambda (data)
                                                        (deps:send-msg (notification:stdout data)))
                                          :stderr-fn (lambda (data)
                                                         (deps:send-msg (notification:stderr data))))))

        (when (cdr (assoc :store-result params))
              (state:add-history result))

        (deps:send-msg (handler-utils:result id "text" (format nil "~A" result)))))


(declaim (ftype (function (cons) null) handle))
(defun handle (msg)
    (let ((thread (threads:run-in-thread (cdr (assoc :method msg))
                                         msg
                                         (lambda ()
                                             (process-eval msg)))))
        (signal (make-condition 'eval-thread-event :thread thread))))
