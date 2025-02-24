(defpackage :alive/session/handler/inspect
    (:use :cl)
    (:export :new-do-close
             :new-do-inspect
             :new-do-inspect-eval
             :new-do-symbol
             :new-macro
             :new-refresh
             :new-send-inspect-result
             :new-try-inspect
             :refresh)
    (:local-nicknames (:deps :alive/deps)
                      (:errors :alive/lsp/errors)
                      (:eval :alive/sys/eval)
                      (:inspector :alive/inspector)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:macros :alive/macros)
                      (:notification :alive/lsp/message/notification)
                      (:state :alive/session/state)
                      (:threads :alive/session/threads)))

(in-package :alive/session/handler/inspect)


(declaim (ftype (function (integer &key (:insp-id integer) (:result *) (:result-type string)) hash-table) inspect-response))
(defun inspect-response (id &key insp-id result result-type)
    (let ((data (make-hash-table :test #'equalp))
          (expr-type (if result-type result-type "expr")))

        (setf (gethash "id" data) insp-id)
        (setf (gethash "resultType" data) expr-type)
        (setf (gethash "result" data) result)

        (lsp-msg:create-response id :result-value data)))


(declaim (ftype (function (deps:dependencies state:state &key (:id integer) (:text string) (:pkg-name string) (:result *) (:result-type string) (:convert boolean)) null) new-send-inspect-result))
(defun new-send-inspect-result (deps state &key id text pkg-name result (convert T) (result-type "expr"))
    (let ((insp-id (state:next-inspector-id state)))
        (state:add-inspector state insp-id
                                 (inspector:create :text text
                                                   :pkg pkg-name
                                                   :result result))

        (deps:send-msg deps (inspect-response id
                                                  :insp-id insp-id
                                                  :result-type result-type
                                                  :result (if convert
                                                              (inspector:to-result result)
                                                              (princ-to-string result))))))


(declaim (ftype (function (deps:dependencies state:state integer string string) null) new-try-inspect))
(defun new-try-inspect (deps state id text pkg-name)
    (let ((result (eval:new-from-string deps text
                                        :pkg-name pkg-name
                                        :stdin-fn (lambda ()
                                                      (threads:wait-for-input deps state))
                                        :stdout-fn (lambda (data)
                                                       (deps:send-msg deps (notification:stdout data)))
                                        :stderr-fn (lambda (data)
                                                       (deps:send-msg deps (notification:stderr data))))))

        (new-send-inspect-result deps state
                                 :id id
                                 :text text
                                 :pkg-name pkg-name
                                 :result result)))


(declaim (ftype (function (deps:dependencies state:state list) null) new-do-inspect))
(defun new-do-inspect (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (or (cdr (assoc :package params))
                         "cl-user"))
           (text (cdr (assoc :text params)))
           (* (state:get-history-item state 0))
           (** (state:get-history-item state 1))
           (*** (state:get-history-item state 2)))
        (handler-case
                (progn (unless (stringp text)
                           (error "No text to inspect"))
                       (new-try-inspect deps state id text pkg-name))
            (T (c)
               (deps:send-msg deps (lsp-msg:create-error id
                                                             :code errors:*internal-error*
                                                             :message (princ-to-string c)))))))


(declaim (ftype (function (deps:dependencies state:state list) null) new-do-inspect-eval))
(defun new-do-inspect-eval (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (insp-id (cdr (assoc :id params)))
           (text (or (cdr (assoc :text params)) "nil"))
           (inspector (when insp-id (state:get-inspector state insp-id)))
           (old-result (inspector:get-result inspector))
           (* (if (symbolp old-result)
                  (symbol-value old-result)
                  old-result))
           (pkg-name (inspector:get-pkg inspector))
           (new-result (eval:new-from-string deps text
                                             :pkg-name pkg-name
                                             :stdin-fn (lambda ()
                                                           (threads:wait-for-input deps state))
                                             :stdout-fn (lambda (data)
                                                            (deps:send-msg deps (notification:stdout data)))
                                             :stderr-fn (lambda (data)
                                                            (deps:send-msg deps (notification:stderr data))))))

        (if new-result
            (new-send-inspect-result deps state
                                     :id id
                                     :text text
                                     :pkg-name pkg-name
                                     :result new-result)

            (deps:send-msg deps (lsp-msg:create-response id
                                                             :result-value (make-hash-table))))))


(declaim (ftype (function (deps:dependencies state:state list) null) new-refresh))
(defun new-refresh (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (insp-id (cdr (assoc :id params)))
           (inspector (when insp-id (state:get-inspector state insp-id)))
           (result (inspector:get-result inspector)))

        (typecase result
            (symbol (deps:send-msg deps (inspect-response id
                                                              :insp-id insp-id
                                                              :result (inspector:to-result (if (fboundp result)
                                                                                               result
                                                                                               (symbol-value result))))))
            (otherwise (deps:send-msg deps (inspect-response id
                                                                 :insp-id insp-id
                                                                 :result (inspector:to-result result)))))))


(declaim (ftype (function (state:state list) hash-table) new-do-close))
(defun new-do-close (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (insp-id (cdr (assoc :id params))))

        (state:rem-inspector state insp-id)
        (lsp-msg:create-response id :result-value T)))


(declaim (ftype (function (deps:dependencies state:state list) null) new-do-symbol))
(defun new-do-symbol (deps state msg)
    (let ((id (cdr (assoc :id msg))))
        (handler-case
                (let* ((params (cdr (assoc :params msg)))
                       (pkg-name (cdr (assoc :package params)))
                       (name (cdr (assoc :symbol params)))
                       (sym (alive/symbols:lookup name pkg-name)))

                    (new-send-inspect-result deps state
                                             :id id
                                             :text name
                                             :pkg-name pkg-name
                                             :result sym))

            (T (c)
               (deps:send-msg deps (lsp-msg:create-error id
                                                             :code errors:*internal-error*
                                                             :message (princ-to-string c)))))))


(declaim (ftype (function (deps:dependencies state:state list) null) new-macro))
(defun new-macro (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (text (cdr (assoc :text params)))
           (expanded (macros:expand-1 text pkg-name)))

        (new-send-inspect-result deps state
                                 :id id
                                 :text text
                                 :pkg-name pkg-name
                                 :result-type "macro"
                                 :convert NIL
                                 :result expanded)))
