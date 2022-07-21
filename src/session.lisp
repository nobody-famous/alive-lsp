(defpackage :alive/session
    (:use :cl)
    (:export :add-listener
             :create
             :listener
             :start
             :stop)
    (:local-nicknames (:completion :alive/lsp/message/document/completion)
                      (:did-open :alive/lsp/message/document/did-open)
                      (:did-change :alive/lsp/message/document/did-change)
                      (:hover :alive/lsp/message/document/hover)
                      (:formatting :alive/lsp/message/document/range-format)
                      (:format-utils :alive/lsp/message/document/format-utils)
                      (:fmt-on-type :alive/lsp/message/document/fmt-on-type)
                      (:config :alive/lsp/message/workspace/config)
                      (:input :alive/lsp/message/alive/user-input)
                      (:asdf :alive/asdf)
                      (:eval :alive/eval)
                      (:inspector :alive/inspector)
                      (:file :alive/file)
                      (:pos :alive/position)
                      (:packages :alive/packages)
                      (:threads :alive/threads)
                      (:formatter :alive/format)
                      (:tokenizer :alive/parse/tokenizer)
                      (:analysis :alive/lsp/sem-analysis)
                      (:init :alive/lsp/message/initialize)
                      (:form :alive/parse/form)
                      (:forms :alive/parse/forms)
                      (:debug :alive/lsp/message/alive/debugger)
                      (:eval-msg :alive/lsp/message/alive/do-eval)
                      (:inspect-msg :alive/lsp/message/alive/do-inspect)
                      (:inspect-sym-msg :alive/lsp/message/alive/do-inspect-sym)
                      (:inspect-close-msg :alive/lsp/message/alive/do-inspect-close)
                      (:get-pkg :alive/lsp/message/alive/get-pkg)
                      (:remove-pkg :alive/lsp/message/alive/remove-pkg)
                      (:load-asdf :alive/lsp/message/alive/load-asdf)
                      (:list-asdf :alive/lsp/message/alive/list-asdf)
                      (:list-pkgs :alive/lsp/message/alive/list-packages)
                      (:list-threads :alive/lsp/message/alive/list-threads)
                      (:kill-thread :alive/lsp/message/alive/kill-thread)
                      (:load-file :alive/lsp/message/alive/load-file)
                      (:symbol :alive/lsp/message/alive/symbol)
                      (:try-compile :alive/lsp/message/alive/try-compile)
                      (:unexport :alive/lsp/message/alive/unexport-symbol)
                      (:stderr :alive/lsp/message/alive/stderr)
                      (:stdout :alive/lsp/message/alive/stdout)
                      (:top-form :alive/lsp/message/alive/top-form)
                      (:logger :alive/logger)
                      (:message :alive/lsp/message/abstract)
                      (:comps :alive/lsp/completions)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:errors :alive/lsp/errors)
                      (:config-item :alive/lsp/types/config-item)
                      (:text-doc :alive/lsp/types/text-doc)
                      (:fmt-opts :alive/lsp/types/format-options)
                      (:debug-resp :alive/lsp/types/debug-resp)
                      (:user-input :alive/lsp/types/user-input)
                      (:restart-info :alive/lsp/types/restart-info)
                      (:sem-tokens :alive/lsp/message/document/sem-tokens-full)))

(in-package :alive/session)


(defclass listener ()
        ((on-done :accessor on-done
                  :initform nil
                  :initarg :on-done)))


(defclass state ()
        ((running :accessor running
                  :initform T
                  :initarg :running)
         (initialized :accessor initialized
                      :initform nil
                      :initarg :initialized)
         (files :accessor files
                :initform (make-hash-table :test 'equalp)
                :initarg :files)
         (thread-msgs :accessor thread-msgs
                      :initform (make-hash-table :test 'equalp)
                      :initarg :thread-msgs)
         (listeners :accessor listeners
                    :initform nil
                    :initarg :listeners)
         (thread-name-id :accessor thread-name-id
                         :initform 1
                         :initarg :thread-name-id)
         (lock :accessor lock
               :initform (bt:make-recursive-lock)
               :initarg :lock)
         (send-msg-id :accessor send-msg-id
                      :initform 1
                      :initarg :send-msg-id)
         (sent-msg-callbacks :accessor sent-msg-callbacks
                             :initform (make-hash-table :test 'equalp)
                             :initarg :sent-msg-callbacks)
         (inspector-id :accessor inspector-id
                       :initform 1
                       :initarg :inspector-id)
         (inspectors :accessor inspectors
                     :initform (make-hash-table :test 'equalp)
                     :initarg :inspectors)
         (input-cond-vars :accessor input-cond-vars
                          :initform (make-hash-table :test 'equalp)
                          :initarg :input-cond-vars)
         (history :accessor history
                  :initform (make-array 3)
                  :initarg :history)
         (read-thread :accessor read-thread
                      :initform nil
                      :initarg :read-thread)))


(defun add-history (state item)
    (setf (elt (history state) 2)
        (elt (history state) 1))
    (setf (elt (history state) 1)
        (elt (history state) 0))
    (setf (elt (history state) 0)
        item))


(defclass network-state (state)
        ((conn :accessor conn
               :initform nil
               :initarg :conn)))


(defun create (&key conn)
    (make-instance 'network-state
        :conn conn
        :running nil
        :listeners nil
        :read-thread nil))


(defmethod destroy ((obj network-state))
    (when (conn obj)
          (usocket:socket-close (conn obj))
          (setf (conn obj) NIL)))


(defmethod add-listener ((obj state) (to-add listener))
    (push to-add (listeners obj)))


(defmethod set-initialized ((obj state) value)
    (setf (initialized obj) value))


(defmethod set-file-text ((obj state) uri text)
    (setf (gethash uri (files obj)) text))


(defmethod get-file-text ((obj state) uri)
    (gethash uri (files obj)))


(defmethod next-send-id ((obj state))
    (bt:with-recursive-lock-held ((lock obj))
        (let ((id (send-msg-id obj)))
            (incf (send-msg-id obj))
            id)))


(defmethod next-inspector-id ((obj state))
    (bt:with-recursive-lock-held ((lock obj))
        (let ((id (inspector-id obj)))
            (incf (inspector-id obj))
            id)))


(defmethod add-inspector ((obj state) &key id inspector)
    (bt:with-recursive-lock-held ((lock obj))
        (setf (gethash id (inspectors obj))
            inspector)))


(defmethod rem-inspector ((obj state) &key id)
    (bt:with-recursive-lock-held ((lock obj))
        (remhash id (inspectors obj))))


(defmethod get-input-stream ((obj network-state))
    (flexi-streams:make-flexi-stream
        (usocket:socket-stream (conn obj))))


(defmethod get-output-stream ((obj network-state))
    (usocket:socket-stream (conn obj)))


(defmethod send-msg ((obj network-state) msg)
    (when (logger:has-level logger:*trace*)
          (logger:msg logger:*trace* "<-- ~A~%" (json:encode-json-to-string msg)))

    (bt:with-recursive-lock-held ((lock obj))
        (write-sequence (flexi-streams:string-to-octets (packet:to-wire msg)) (usocket:socket-stream (conn obj)))
        (force-output (usocket:socket-stream (conn obj)))))


(defun save-thread-msg (state msg)
    (let* ((table (thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (threads:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((lock state))
            (setf (gethash thread-id table) (message:id msg)))))


(defun rem-thread-msg (state)
    (let* ((table (thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (threads:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((lock state))
            (remhash thread-id table))))


(defun run-in-thread (state msg fn)
    (let ((stdout *standard-output*))
        (bt:make-thread (lambda ()
                            (unwind-protect
                                    (let ((*standard-output* stdout))
                                        (save-thread-msg state msg)

                                        (block handler
                                            (handler-bind ((error (lambda (err)
                                                                      (start-debugger state err)
                                                                      (return-from handler))))
                                                (funcall fn)))))
                            (rem-thread-msg state))

                        :name (next-thread-name state (if (typep msg 'message:request)
                                                          (message:method-name msg)
                                                          "response")))))


(defun cancel-thread (state thread-id msg-id)
    (when msg-id
          (send-msg state
                    (message:create-error-resp :id msg-id
                                               :code errors:*request-cancelled*
                                               :message (format nil "Request ~A canceled" msg-id))))

    (when thread-id
          (threads:kill thread-id)))


(defmethod handle-msg ((obj state) (msg init:request))
    (let* ((resp (init:create-response (message:id msg))))
        (send-msg obj resp)))


(defmethod handle-msg ((obj state) (msg init:initialized))
    (set-initialized obj T))


(defmethod handle-msg ((obj state) (msg did-open:did-open))
    (let ((uri (did-open:get-uri msg))
          (text (did-open:get-text msg)))

        (when text
              (bt:with-recursive-lock-held ((lock obj))
                  (set-file-text obj uri text)))))


(defmethod handle-msg (state (msg did-change:did-change))
    (let ((uri (did-change:get-uri msg))
          (text (did-change:get-text msg)))

        (when text
              (bt:with-recursive-lock-held ((lock state))
                  (set-file-text state uri text)))))


(defmethod handle-msg (state (msg sem-tokens:request))
    (let* ((params (message:params msg))
           (doc (sem-tokens:text-document params))
           (uri (text-doc:uri doc))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (sem-tokens (analysis:to-sem-tokens
                           (tokenizer:from-stream
                               (make-string-input-stream text)))))

        (send-msg state (sem-tokens:create-response (message:id msg) sem-tokens))))


(defmethod handle-msg (state (msg load-file:request))
    (run-in-thread state msg (lambda ()
                                 (let* ((path (load-file:get-path msg))
                                        (msgs (file:do-load path
                                                            :stdout-fn (lambda (data)
                                                                           (when (load-file:show-stdout-p msg)
                                                                                 (send-msg state (stdout:create data))))
                                                            :stderr-fn (lambda (data)
                                                                           (when (load-file:show-stderr-p msg)
                                                                                 (send-msg state (stderr:create data))))))
                                        (resp (load-file:create-response (message:id msg) msgs)))

                                     (send-msg state resp)))))


(defmethod handle-msg (state (msg try-compile:request))
    (let* ((path (try-compile:get-path msg))
           (msgs (file:try-compile path))
           (resp (try-compile:create-response (message:id msg) msgs)))

        (send-msg state resp)))


(defmethod handle-msg (state (msg completion:request))
    (let* ((params (message:params msg))
           (doc (completion:text-document params))
           (pos (completion:pos params))
           (uri (text-doc:uri doc))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (items (comps:simple :text text :pos pos)))

        (send-msg state (completion:create-response
                            :id (message:id msg)
                            :items items))))


(defmethod handle-msg (state (msg hover:request))
    (let* ((params (message:params msg))
           (doc (hover:text-document params))
           (pos (hover:pos params))
           (uri (text-doc:uri doc))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (hov-text (alive/lsp/hover:get-text :text text :pos pos))
           (result (if hov-text hov-text "")))

        (send-msg state (hover:create-response
                            :id (message:id msg)
                            :value result))))


(defmethod handle-msg (state (msg symbol:request))
    (let* ((params (message:params msg))
           (doc (symbol:text-document params))
           (pos (symbol:pos params))
           (uri (text-doc:uri doc))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (result (alive/lsp/symbol:for-pos :text text :pos pos)))

        (send-msg state (symbol:create-response
                            :id (message:id msg)
                            :value result))))


(defmethod handle-msg (state (msg top-form:request))
    (let* ((params (message:params msg))
           (doc (top-form:text-document params))
           (pos (top-form:pos params))
           (uri (text-doc:uri doc))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (forms (forms:from-stream (make-string-input-stream text))))

        (loop :with start := nil
              :with end := nil

              :for form :in forms :do
                  (when (and (pos:less-or-equal (form:get-start form) pos)
                             (pos:less-or-equal pos (form:get-end form)))
                        (setf start (form:get-start form))
                        (setf end (form:get-end form)))

              :finally (send-msg state (top-form:create-response :id (message:id msg)
                                                                 :start start
                                                                 :end end)))))


(defun handle-format-msg (state options msg)
    (let* ((params (message:params msg))
           (range (formatting:range params))
           (doc (formatting:text-document params))
           (uri (text-doc:uri doc))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (edits (formatter:range (make-string-input-stream text)
                                   range
                                   options)))

        (send-msg state (format-utils:create-response (message:id msg) edits))))


(defmethod handle-msg (state (msg formatting:request))
    (let ((send-id (next-send-id state)))
        (setf (gethash send-id (sent-msg-callbacks state))
            (lambda (config-resp)
                (let ((opts (when (message:result config-resp)
                                  (fmt-opts:from-wire (message:result config-resp)))))
                    (handle-format-msg state opts msg))))

        (send-msg state (config:create-request
                            :id send-id
                            :params (config:create-params :items (list (config-item:create-item :section "alive.format")))))))


(defmethod handle-msg (state (msg fmt-on-type:request))
    (let* ((params (message:params msg))
           (doc (fmt-on-type:text-document params))
           (opts (fmt-on-type:options params))
           (pos (fmt-on-type:pos params))
           (uri (text-doc:uri doc))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (edits (formatter:on-type (make-string-input-stream text)
                                     :options opts
                                     :pos pos)))

        (send-msg state (format-utils:create-response (message:id msg) edits))))


(defmethod handle-msg (state (msg list-threads:request))
    (bt:with-recursive-lock-held ((lock state))
        (let ((threads (remove-if (lambda (thread)
                                      (eq (threads:id thread) (threads:get-thread-id (bt:current-thread))))
                               (threads:list-all))))

            (send-msg state (list-threads:create-response (message:id msg)
                                                          threads)))))


(defmethod handle-msg (state (msg kill-thread:request))
    (handler-case
            (progn
             (cancel-thread state
                            (kill-thread:get-id msg)
                            (gethash (kill-thread:get-id msg)
                                     (thread-msgs state)))

             (send-msg state (kill-thread:create-response (message:id msg))))
        (threads:thread-not-found (c)
                                  (send-msg state
                                            (message:create-error-resp :id (message:id msg)
                                                                       :code errors:*request-failed*
                                                                       :message (format nil "Thread ~A not found" (threads:id c)))))))


(defmethod handle-msg (state (msg list-pkgs:request))
    (send-msg state (list-pkgs:create-response (message:id msg)
                                               (packages:list-all))))


(defmethod handle-msg (state (msg list-asdf:request))
    (send-msg state (list-asdf:create-response (message:id msg)
                                               (asdf:list-systems))))


(defmethod handle-msg (state (msg unexport:request))
    (let* ((sym-name (unexport:get-symbol msg))
           (pkg-name (unexport:get-package msg)))

        (packages:unexport-symbol pkg-name sym-name)
        (send-msg state (unexport:create-response (message:id msg)))))


(defun wait-for-input (state)
    (let ((send-id (next-send-id state))
          (cond-var (bt:make-condition-variable))
          (text nil))

        (setf (gethash send-id (sent-msg-callbacks state))
            (lambda (input-resp)
                (typecase input-resp
                    (message:error-response (logger:msg logger:*error* "Input Error ~A" input-resp)
                                            (bt:condition-notify cond-var))

                    (message:result-response (let ((opts (when (message:result input-resp)
                                                               (user-input:from-wire (message:result input-resp)))))
                                                 (setf text (user-input:get-text opts))
                                                 (bt:condition-notify cond-var))))))

        (send-msg state (input:create-request :id send-id))

        (bt:with-recursive-lock-held ((lock state))
            (bt:condition-wait cond-var (lock state)))

        text))


(defun process-eval (state msg)
    (let* ((pkg-name (eval-msg:get-package msg))
           (text (eval-msg:get-text msg))
           (* (elt (history state) 0))
           (** (elt (history state) 1))
           (*** (elt (history state) 2))
           (result (eval:from-string text
                                     :pkg-name pkg-name
                                     :stdin-fn (lambda ()
                                                   (wait-for-input state))
                                     :stdout-fn (lambda (data)
                                                    (send-msg state (stdout:create data)))
                                     :stderr-fn (lambda (data)
                                                    (send-msg state (stderr:create data))))))

        (when (eval-msg:store-result-p msg)
              (add-history state result))

        (send-msg state
                  (eval-msg:create-response (message:id msg)
                                            (format nil "~A" result)))))


(defun wait-for-debug (state err restarts)
    (let ((send-id (next-send-id state))
          (cond-var (bt:make-condition-variable))
          (params (debug:create-params :message (format nil "~A" err)
                                       :restarts restarts
                                       :stack-trace (mapcar (lambda (item) (princ-to-string item))
                                                            (threads:get-stack-trace))))
          (restart-ndx nil))

        (setf (gethash send-id (sent-msg-callbacks state))
            (lambda (debug-resp)
                (typecase debug-resp
                    (message:error-response (logger:msg logger:*error* "Debugger Error ~A" debug-resp)
                                            (bt:condition-notify cond-var))

                    (message:result-response (let ((opts (when (message:result debug-resp)
                                                               (debug-resp:from-wire (message:result debug-resp)))))
                                                 (setf restart-ndx (debug-resp:get-index opts))
                                                 (bt:condition-notify cond-var))))))

        (send-msg state (debug:create-request :id send-id :params params))

        (bt:with-recursive-lock-held ((lock state))
            (bt:condition-wait cond-var (lock state)))

        restart-ndx))


(defun start-debugger (state err)
    (let* ((restarts (compute-restarts err))
           (ndx (wait-for-debug state err (mapcar (lambda (item)
                                                      (restart-info:create-item :name (restart-name item)
                                                                                :description (princ-to-string item)))
                                                  restarts))))

        (when (<= 0 ndx (- (length restarts) 1))
              (invoke-restart-interactively (elt restarts ndx)))))


(defmethod handle-msg (state (msg eval-msg:request))
    (run-in-thread state msg (lambda ()
                                 (process-eval state msg))))


(defun try-inspect (state id text pkg-name)
    (let ((result (eval:from-string text
                                    :pkg-name pkg-name
                                    :stdin-fn (lambda ()
                                                  (wait-for-input state))
                                    :stdout-fn (lambda (data)
                                                   (send-msg state (stdout:create data)))
                                    :stderr-fn (lambda (data)
                                                   (send-msg state (stderr:create data))))))

        (add-inspector state
                       :id id
                       :inspector (inspector:create :text text
                                                    :pkg pkg-name
                                                    :result result))

        (inspector:to-result result)))


(defun process-inspect (state msg)
    (handler-case
            (let* ((pkg-name (inspect-msg:get-package msg))
                   (text (inspect-msg:get-text msg))
                   (id (next-inspector-id state))
                   (* (elt (history state) 0))
                   (** (elt (history state) 1))
                   (*** (elt (history state) 2)))

                (let ((result (try-inspect state id text pkg-name)))
                    (send-msg state
                              (inspect-msg:create-response (message:id msg)
                                                           id
                                                           result))))
        (T (c)
           (send-msg state
                     (message:create-error-resp :code errors:*internal-error*
                                                :message (format nil "~A" c)
                                                :id (message:id msg))))))


(defmethod handle-msg (state (msg inspect-msg:request))
    (run-in-thread state msg (lambda ()
                                 (process-inspect state msg))))


(defun process-inspect-sym (state msg)
    (handler-case
            (let* ((pkg-name (inspect-sym-msg:get-package msg))
                   (name (inspect-sym-msg:get-symbol msg))
                   (id (next-inspector-id state))
                   (sym (alive/symbols:lookup name pkg-name))
                   (result (inspector:to-result sym)))

                (add-inspector state
                               :id id
                               :inspector (inspector:create :text name
                                                            :pkg pkg-name
                                                            :result result))

                (send-msg state
                          (inspect-msg:create-response (message:id msg)
                                                       id
                                                       result)))
        (T (c)
           (send-msg state
                     (message:create-error-resp :code errors:*internal-error*
                                                :message (format nil "~A" c)
                                                :id (message:id msg))))))


(defmethod handle-msg (state (msg inspect-sym-msg:request))
    (run-in-thread state msg (lambda ()
                                 (process-inspect-sym state msg))))


(defmethod handle-msg (state (msg inspect-close-msg:request))
    (let ((insp-id (inspect-close-msg:get-id msg)))
        (rem-inspector state :id insp-id)
        (send-msg state
                  (message:create-result-resp :id (message:id msg)
                                              :result "OK"))))


(defmethod handle-msg (state (msg get-pkg:request))
    (let* ((params (message:params msg))
           (doc (get-pkg:text-document params))
           (pos (get-pkg:pos params))
           (uri (text-doc:uri doc))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (pkg (packages:for-pos text pos)))

        (send-msg state (get-pkg:create-response :id (message:id msg)
                                                 :pkg-name pkg))))


(defmethod handle-msg (state (msg remove-pkg:request))
    (let* ((params (message:params msg))
           (pkg-name (remove-pkg:name params)))

        (packages:do-remove pkg-name)
        (send-msg state (remove-pkg:create-response :id (message:id msg)))))


(defmethod handle-msg (state (msg load-asdf:request))
    (let* ((params (message:params msg))
           (name (load-asdf:get-name params)))

        (run-in-thread state msg (lambda ()
                                     (asdf:load-system :name name
                                                       :stdout-fn (lambda (data)
                                                                      (send-msg state (stdout:create data)))
                                                       :stderr-fn (lambda (data)
                                                                      (send-msg state (stderr:create data))))

                                     (send-msg state (load-asdf:create-response (message:id msg)))))))


(defmethod handle-msg (state (msg message:response))
    (let* ((msg-id (message:id msg))
           (cb (gethash msg-id (sent-msg-callbacks state))))

        (if cb
            (funcall cb msg)
            (message:create-error-resp :id (message:id msg)
                                       :code errors:*request-failed*
                                       :message (format nil "No callback for request: ~A" (message:id msg))))))


(defun stop (state)
    (when (logger:has-level logger:*info*)
          (logger:msg logger:*info* "Stopping state ~A" state))

    (setf (running state) NIL)

    (destroy state)

    (loop :for listener :in (listeners state) :do
              (when (on-done listener)
                    (funcall (on-done listener)))))


(defun read-message (state &optional use-new-api)
    (handler-case

            (if use-new-api
                (parse:from-stream-new (get-input-stream state))
                (parse:from-stream (get-input-stream state)))

        (end-of-file (c)
                     (declare (ignore c))
                     (stop state))

        (errors:unhandled-request (c)
                                  (logger:msg logger:*error* "read-message: ~A" c)
                                  (when (errors:id c)
                                        (send-msg state
                                                  (message:create-error-resp :id (errors:id c)
                                                                             :code errors:*method-not-found*
                                                                             :message (format nil "Unhandled request: ~A" (errors:method-name c))))))

        (errors:server-error (c)
                             (logger:msg logger:*error* "read-message: ~A" c)
                             (when (errors:id c)
                                   (send-msg state
                                             (message:create-error-resp :id (errors:id c)
                                                                        :code errors:*internal-error*
                                                                        :message (format nil "Server error: ~A" (errors:message c))))))

        (T (c)
           (logger:msg logger:*error* "read-message: ~A" c)
           (stop state))))


(defun next-thread-name (state method-name)
    (let ((name (format nil "~A - ~A" (thread-name-id state) method-name)))
        (incf (thread-name-id state))
        name))


(defun handle-init (state msg)
    (declare (ignore state))
    (let ((resp (message:create-response (cdr (assoc :id msg))
                                         :result-value (list (cons :foo "bar")))))
        (loop :for item :in resp
              :do (format T "ITEM ~A~%" item))))


(defparameter *handlers* (list (cons "initialize" 'handle-init)))


(defun handle-msg-new (state msg)
    (let* ((method-name (cdr (assoc :method msg)))
           (handler (cdr (assoc method-name *handlers* :test #'string=))))
        (if handler
            (funcall handler state msg)
            (error (format nil "No handler for ~A" method-name)))))


(defun process-msg (state msg)
    (unwind-protect
            (handler-case

                    (progn (when (typep msg 'message:request)
                                 (save-thread-msg state msg))

                           (when (logger:has-level logger:*trace*)
                                 (logger:msg logger:*trace* "--> ~A~%" (json:encode-json-to-string msg)))

                           (handle-msg state msg))

                (error (c)
                    (logger:msg logger:*error* "Message Handler: ~A" c)
                    (send-msg state
                              (message:create-error-resp :code errors:*internal-error*
                                                         :message (format nil "~A" c)
                                                         :id (message:id msg)))))
        (when (typep msg 'message:request)
              (rem-thread-msg state))))


(defun read-messages (state)
    (loop :while (running state)
          :do (let ((msg (read-message state)))
                  (when msg
                        (process-msg state msg)))))


(defun start-read-thread (state)
    (let ((stdout *standard-output*))
        (setf (read-thread state)
            (bt:make-thread (lambda ()
                                (let ((*standard-output* stdout))
                                    (read-messages state)))
                            :name "Session Message Reader"))))


(defun start (state)
    (setf (running state) T)

    (start-read-thread state)

    (when (logger:has-level logger:*info*)
          (logger:msg logger:*info* "Started state ~A" state)))