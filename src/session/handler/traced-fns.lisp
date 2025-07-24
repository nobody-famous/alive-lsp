(defpackage :alive/session/handler/traced-fns
    (:use :cl)
    (:export :list-all
             :trace-fn)
    (:local-nicknames (:deps :alive/deps)
                      (:lsp-msg :alive/lsp/message/abstract)
                      (:packages :alive/packages)
                      (:pos :alive/position)
                      (:state :alive/session/state)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)
                      (:utils :alive/session/handler/utils)))

(in-package :alive/session/handler/traced-fns)


(declaim (ftype (function (string cons) (values (or null string) (or null string))) get-function-for-pos))
(defun get-function-for-pos (text pos)
    (loop :with done := nil
          :with fn-name := nil
          :with pkg-name := nil
          :with prev := nil
          :with next := nil
          :with token := nil
          :with tokens := (tokenizer:from-stream (make-string-input-stream text))

          :while (and tokens (not done))
          :do (setf token (car tokens))
              (setf tokens (cdr tokens))
              (setf next (car tokens))

              (cond ((pos:less-than pos (token:get-end token))
                        (when (and (eq types:*symbol* (token:get-type-value token))
                                   (not (eq types:*colons* (token:get-type-value next)))
                                   (not (and (eq types:*colons* (token:get-type-value prev))
                                             (not pkg-name))))
                              (setf fn-name (token:get-text token)))
                        (unless fn-name
                            (setf pkg-name nil))
                        (setf done T))
                    ((eq types:*colons* (token:get-type-value token))
                        (when (and prev (eq types:*symbol* (token:get-type-value prev)))
                              (setf pkg-name (token:get-text prev))))
                    (T (setf pkg-name nil)))

              (setf prev token)

          :finally (return (if fn-name
                               (values pkg-name fn-name)
                               (values nil nil)))))


(declaim (ftype (function (deps:dependencies(or null string) (or null string)) boolean) do-trace-fn))
(defun do-trace-fn (deps pkg-name fn-name)
    (let ((pkg (if pkg-name
                   (packages:for-string pkg-name)
                   *package*)))
        (when pkg
              (let ((*package* pkg))
                  (deps:trace-fn deps fn-name)))))


(declaim (ftype (function (deps:dependencies state:state cons) hash-table) trace-fn))
(defun trace-fn (deps state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (text (or (state:get-file-text state uri) "")))

        (multiple-value-bind (pkg-name fn-name)
                (get-function-for-pos text pos)
            (when fn-name
                  (do-trace-fn deps pkg-name fn-name))
            (lsp-msg:create-response id :result-value T))))


(declaim (ftype (function (deps:dependencies cons) hash-table) list-all))
(defun list-all (deps msg)
    (utils:result (cdr (assoc :id msg))
                  "traced"
                  (deps:list-all-traced deps)))
