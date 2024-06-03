(defpackage :alive/session
    (:use :cl)
    (:export :start
             :stop)
    (:local-nicknames (:logger :alive/logger)
                      (:state :alive/session/state)
                      (:utils :alive/thread-utils)))

(in-package :alive/session)


; (defun send-inspect-result (state &key id text pkg-name result (convert T) (result-type "expr"))
;     (let ((insp-id (next-inspector-id state)))
;         (add-inspector state
;                        :id insp-id
;                        :inspector (inspector:create :text text
;                                                     :pkg pkg-name
;                                                     :result result))

;         (send-msg state
;                   (resp:do-inspect id
;                                    :insp-id insp-id
;                                    :result-type result-type
;                                    :result (if convert
;                                                (inspector:to-result result)
;                                                (princ-to-string result))))))


; (defun try-inspect (state id text pkg-name)
;     (let ((result (eval:from-string text
;                                     :pkg-name pkg-name
;                                     :stdin-fn (lambda ()
;                                                   (wait-for-input state))
;                                     :stdout-fn (lambda (data)
;                                                    (send-msg state (notification:stdout data)))
;                                     :stderr-fn (lambda (data)
;                                                    (send-msg state (notification:stderr data))))))

;         (send-inspect-result state
;                              :id id
;                              :text text
;                              :pkg-name pkg-name
;                              :result result)))


; (defun process-inspect (state msg)
;     (let ((id (cdr (assoc :id msg))))

;         (handler-case
;                 (let* ((params (cdr (assoc :params msg)))
;                        (pkg-name (cdr (assoc :package params)))
;                        (text (cdr (assoc :text params)))
;                        (* (elt (history state) 0))
;                        (** (elt (history state) 1))
;                        (*** (elt (history state) 2)))

;                     (try-inspect state id text pkg-name))

;             (T (c)
;                (send-msg state
;                          (message:create-error id
;                                                :code errors:*internal-error*
;                                                :message (princ-to-string c)))))))


; (defun handle-inspect (state msg)
;     (run-in-thread state msg (lambda ()
;                                  (process-inspect state msg))))


; (defun process-inspect-sym (state msg)
;     (let ((id (cdr (assoc :id msg))))

;         (handler-case
;                 (let* ((params (cdr (assoc :params msg)))
;                        (pkg-name (cdr (assoc :package params)))
;                        (name (cdr (assoc :symbol params)))
;                        (sym (alive/symbols:lookup name pkg-name)))

;                     (send-inspect-result state
;                                          :id id
;                                          :text name
;                                          :pkg-name pkg-name
;                                          :result sym))

;             (T (c)
;                (send-msg state (message:create-error id
;                                                      :code errors:*internal-error*
;                                                      :message (princ-to-string c)))))))


; (defun handle-inspect-sym (state msg)
;     (run-in-thread state msg (lambda ()
;                                  (process-inspect-sym state msg))))


; (defun handle-inspect-close (state msg)
;     (let* ((id (cdr (assoc :id msg)))
;            (params (cdr (assoc :params msg)))
;            (insp-id (cdr (assoc :id params))))

;         (rem-inspector state :id insp-id)
;         (message:create-response id :result-value T)))


; (defun do-inspect-eval (state msg)
;     (let* ((id (cdr (assoc :id msg)))
;            (params (cdr (assoc :params msg)))
;            (insp-id (cdr (assoc :id params)))
;            (text (cdr (assoc :text params)))
;            (inspector (get-inspector state :id insp-id))
;            (old-result (inspector:get-result inspector))
;            (* (if (symbolp old-result)
;                   (symbol-value old-result)
;                   old-result))
;            (pkg-name (inspector:get-pkg inspector))
;            (new-result (eval:from-string text
;                                          :pkg-name pkg-name
;                                          :stdin-fn (lambda ()
;                                                        (wait-for-input state))
;                                          :stdout-fn (lambda (data)
;                                                         (send-msg state (notification:stdout data)))
;                                          :stderr-fn (lambda (data)
;                                                         (send-msg state (notification:stderr data))))))

;         (if new-result
;             (send-inspect-result state
;                                  :id id
;                                  :text text
;                                  :pkg-name pkg-name
;                                  :result new-result)

;             (send-msg state (message:create-response id
;                                                      :result-value (make-hash-table))))))


; (defun handle-inspect-eval (state msg)
;     (run-in-thread state msg (lambda ()
;                                  (do-inspect-eval state msg))))


; (defun handle-inspect-macro (state msg)
;     (let* ((id (cdr (assoc :id msg)))
;            (params (cdr (assoc :params msg)))
;            (pkg-name (cdr (assoc :package params)))
;            (text (cdr (assoc :text params)))
;            (expanded (macros:expand-1 text pkg-name)))

;         (send-inspect-result state
;                              :id id
;                              :text text
;                              :pkg-name pkg-name
;                              :result-type "macro"
;                              :convert NIL
;                              :result expanded)))


(defun stop ()
    (logger:info-msg "Stopping session")

    (state:set-running nil)

    (alive/context:destroy)

    #+n (loop :for listener :in (state:listeners)
              :do (when (on-done listener)
                        (funcall (on-done listener)))))


(declaim (ftype (function () null) start))
(defun start ()
    (state:with-state (state:create)
        (state:add-listener (state:create-listener (lambda () (alive/context:destroy))))
        (state:set-running T)

        (utils:spawn-thread "Session Message Reader"
            (alive/session/message-loop:run))

        (logger:info-msg "Session started")
        nil))
