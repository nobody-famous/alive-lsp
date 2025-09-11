(defpackage :alive/session/handler/traced-fns
    (:use :cl)
    (:export :list-all
             :trace-fn
             :trace-pkg
             :untrace-fn
             :untrace-fn-by-name
             :untrace-pkg)
    (:local-nicknames (:deps :alive/deps)
                      (:logger :alive/logger)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:packages :alive/packages)
                      (:pos :alive/position)
                      (:state :alive/session/state)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/traced-fns)


(declaim (ftype (function (string cons) (values (or null string) (or null string))) get-function-for-pos))
(defun get-function-for-pos (text pos)
    (loop :with done := nil
          :with to-trace := nil
          :with fn-name := nil
          :with pkg-name := nil
          :with colons := nil
          :with prev := nil
          :with next := nil
          :with next-next := nil
          :with token := nil
          :with tokens := (tokenizer:from-stream (make-string-input-stream text))

          :while (and tokens (not done))
          :do (setf token (car tokens))
              (setf tokens (cdr tokens))
              (setf next (car tokens))
              (setf next-next (car (cdr tokens)))

              (cond ((pos:less-than pos (token:get-end token))
                        (cond ((eq types:*symbol* (token:get-type-value token))
                                  (cond ((and (not (eq types:*colons* (token:get-type-value next)))
                                              (not (and (eq types:*colons* (token:get-type-value prev))
                                                        (not pkg-name))))
                                            (setf fn-name (token:get-text token)))
                                        ((and (not pkg-name)
                                              (eq types:*colons* (token:get-type-value next))
                                              (eq types:*symbol* (token:get-type-value next-next)))
                                            (setf pkg-name (token:get-text token))
                                            (setf colons (token:get-text next))
                                            (setf fn-name (token:get-text next-next)))))
                              ((and (eq types:*colons* (token:get-type-value token))
                                    (eq types:*symbol* (token:get-type-value prev))
                                    (eq types:*symbol* (token:get-type-value next)))
                                  (setf pkg-name (token:get-text prev))
                                  (setf colons (token:get-text token))
                                  (setf fn-name (token:get-text next))))

                        (unless fn-name
                            (setf pkg-name nil))

                        (setf done T))
                    ((eq types:*colons* (token:get-type-value token))
                        (when (and prev (eq types:*symbol* (token:get-type-value prev)))
                              (setf colons (token:get-text token))
                              (setf pkg-name (token:get-text prev))))
                    (T (setf pkg-name nil)
                       (setf colons nil)))

              (setf prev token)

          :finally (return (cond ((and pkg-name colons fn-name) (format nil "~A~A~A" pkg-name colons fn-name))
                                 (fn-name fn-name)
                                 (T nil)))))


(declaim (ftype (function (deps:dependencies state:state cons) null) trace-fn))
(defun trace-fn (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (pkg (packages:for-pos text pos))
           (*package* (or (packages:lookup pkg) *package*))
           (to-trace (get-function-for-pos text pos))
           (result (if to-trace
                       (deps:trace-fn deps to-trace)
                       nil)))

        (deps:send-msg deps (utils:result id "function" result))))


(declaim (ftype (function (deps:dependencies state:state cons) null) untrace-fn))
(defun untrace-fn (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) ""))
           (pkg (packages:for-pos text pos))
           (*package* (or (packages:lookup pkg) *package*))
           (to-untrace (get-function-for-pos text pos)))

        (when to-untrace
              (deps:untrace-fn deps to-untrace))
        (deps:send-msg deps (utils:result id "function" to-untrace))))


(declaim (ftype (function (deps:dependencies cons) null) untrace-fn-by-name))
(defun untrace-fn-by-name (deps msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (to-untrace (cdr (assoc :function params)))
           (pkg (packages:lookup pkg-name)))

        (when (and to-untrace pkg)
              (let ((*package* pkg))
                  (deps:untrace-fn deps to-untrace)))
        (deps:send-msg deps (lsp-msg:create-response id :result-value T))))


(declaim (ftype (function (deps:dependencies cons) null) trace-pkg))
(defun trace-pkg (deps msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (result (if pkg-name
                       (deps:trace-pkg deps pkg-name)
                       nil)))

        (deps:send-msg deps (utils:result id "function" result))))


(declaim (ftype (function (deps:dependencies cons) null) untrace-pkg))
(defun untrace-pkg (deps msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params))))

        (when pkg-name
              (deps:untrace-pkg deps pkg-name))
        (deps:send-msg deps (lsp-msg:create-response id :result-value T))))


(declaim (ftype (function (deps:dependencies cons) hash-table) list-all))
(defun list-all (deps msg)
    (utils:result (cdr (assoc :id msg))
                  "traced"
                  (mapcar (lambda (item) (list (cons :package (cdr (assoc :package item)))
                                               (cons :name (symbols:add-bars (cdr (assoc :name item))))))
                          (deps:list-all-traced deps))))
