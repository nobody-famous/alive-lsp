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
                      (:formatting :alive/lsp/message/document/range-format)
                      (:config :alive/lsp/message/workspace/config)
                      (:asdf :alive/asdf)
                      (:eval :alive/eval)
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
                      (:eval-msg :alive/lsp/message/alive/do-eval)
                      (:get-pkg :alive/lsp/message/alive/get-pkg)
                      (:remove-pkg :alive/lsp/message/alive/remove-pkg)
                      (:load-asdf :alive/lsp/message/alive/load-asdf)
                      (:list-asdf :alive/lsp/message/alive/list-asdf)
                      (:list-pkgs :alive/lsp/message/alive/list-packages)
                      (:list-threads :alive/lsp/message/alive/list-threads)
                      (:kill-thread :alive/lsp/message/alive/kill-thread)
                      (:load-file :alive/lsp/message/alive/load-file)
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
                      (:sem-tokens :alive/lsp/message/document/sem-tokens-full)))

(in-package :alive/session)


(defclass listener ()
        ((on-done :accessor on-done
                  :initform nil
                  :initarg :on-done)))


(defclass state ()
        ((logger :accessor logger
                 :initform nil
                 :initarg :logger)
         (running :accessor running
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


(defun create (&key logger conn)
    (make-instance 'network-state
        :logger logger
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


(defmethod get-input-stream ((obj network-state))
    (usocket:socket-stream (conn obj)))


(defmethod get-output-stream ((obj network-state))
    (usocket:socket-stream (conn obj)))


(defmethod send-msg ((obj network-state) msg)
    (logger:trace-msg (logger obj) "<-- ~A~%" (json:encode-json-to-string msg))

    (bt:with-recursive-lock-held ((lock obj))
        (write-string (packet:to-wire msg) (usocket:socket-stream (conn obj)))
        (force-output (usocket:socket-stream (conn obj)))))


(defun run-in-thread (state msg fn)
    (let ((stdout *standard-output*))
        (bt:make-thread (lambda ()
                            (let ((*standard-output* stdout))
                                (setf (gethash (threads:get-thread-id (bt:current-thread)) (thread-msgs state))
                                    (message:id msg))

                                (handler-case
                                        (funcall fn)
                                    (T (e)
                                       (send-msg state
                                                 (message:create-error-resp :id (message:id msg)
                                                                            :code errors:*request-failed*
                                                                            :message (format nil "~A" e)))))))

                        :name (next-thread-name state (if (typep msg 'message:request)
                                                          (message:method-name msg)
                                                          "response")))))


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
    (let* ((path (load-file:get-path msg))
           (msgs (file:do-load path
                               :stdout-fn (lambda (data)
                                              (when (load-file:show-stdout-p msg)
                                                  (send-msg state (stdout:create data))))
                               :stderr-fn (lambda (data)
                                              (when (load-file:show-stderr-p msg)
                                                  (send-msg state (stderr:create data))))))
           (resp (load-file:create-response (message:id msg) msgs)))

        (send-msg state resp)))


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

        (send-msg state (formatting:create-response (message:id msg) edits))))


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
                (let ((thread-msg-id (gethash (kill-thread:get-id msg)
                                              (thread-msgs state))))
                    (when thread-msg-id
                        (send-msg state
                                  (message:create-error-resp :id thread-msg-id
                                                             :code errors:*request-cancelled*
                                                             :message (format nil "Request ~A canceled" (message:id msg))))))
                (threads:kill (kill-thread:get-id msg))
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


(defun process-eval (state msg)
    (let* ((pkg-name (eval-msg:get-package msg))
           (text (eval-msg:get-text msg))
           (* (elt (history state) 0))
           (** (elt (history state) 1))
           (*** (elt (history state) 2))
           (result (eval:from-string text
                                     :pkg-name pkg-name
                                     :stdout-fn (lambda (data)
                                                    (send-msg state (stdout:create data)))
                                     :stderr-fn (lambda (data)
                                                    (send-msg state (stderr:create data))))))

        (when (eval-msg:store-result-p msg)
            (add-history state result))

        (send-msg state
                  (eval-msg:create-response (message:id msg)
                                            (format nil "~A" result)))))


(defmethod handle-msg (state (msg eval-msg:request))
    (run-in-thread state msg (lambda ()
                                 (process-eval state msg))))


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

        (asdf:load-system :name name
                          :stdout-fn (lambda (data)
                                         (send-msg state (stdout:create data)))
                          :stderr-fn (lambda (data)
                                         (send-msg state (stderr:create data))))

        (send-msg state (load-asdf:create-response (message:id msg)))))


(defmethod handle-msg (state (msg message:response))
    (let* ((msg-id (message:id msg))
           (cb (gethash msg-id (sent-msg-callbacks state))))

        (if cb
            (funcall cb msg)
            (message:create-error-resp :id (message:id msg)
                                       :code errors:*request-failed*
                                       :message (format nil "No callback for request: ~A" (message:id msg))))))


(defun stop (state)
    (logger:info-msg (logger state) "Stopping state ~A" state)

    (setf (running state) NIL)

    (destroy state)

    (loop :for listener :in (listeners state) :do
        (when (on-done listener)
            (funcall (on-done listener)))))


(defun read-message (state)
    (handler-case

            (parse:from-stream (get-input-stream state))

        (end-of-file (c)
                     (declare (ignore c))
                     (stop state))

        (errors:unhandled-request (c)
                                  (logger:error-msg (logger state) "read-message: ~A" c)
                                  (when (errors:id c)
                                      (send-msg state
                                                (message:create-error-resp :id (errors:id c)
                                                                           :code errors:*method-not-found*
                                                                           :message (format nil "Unhandled request: ~A" (errors:method-name c))))))

        (errors:server-error (c)
                             (logger:error-msg (logger state) "read-message: ~A" c)
                             (when (errors:id c)
                                 (send-msg state
                                           (message:create-error-resp :id (errors:id c)
                                                                      :code errors:*internal-error*
                                                                      :message (format nil "Server error: ~A" (errors:message c))))))

        (T (c)
           (logger:error-msg (logger state) "read-message: ~A" c)
           (stop state))))


(defun next-thread-name (state method-name)
    (let ((name (format nil "~A - ~A" (thread-name-id state) method-name)))
        (incf (thread-name-id state))
        name))


(defun process-msg (state msg)
    (unwind-protect
            (handler-case

                    (progn
                        (when (typep msg 'message:request)
                            (setf (gethash (threads:get-thread-id (bt:current-thread)) (thread-msgs state))
                                (message:id msg)))
                        (logger:trace-msg (logger state) "--> ~A~%" (json:encode-json-to-string msg))
                        (handle-msg state msg))

                (error (c)
                    (logger:error-msg (logger state) "Message Handler: ~A" c)
                    (send-msg state
                              (message:create-error-resp :code errors:*internal-error*
                                                         :message (format nil "~A" c)
                                                         :id (message:id msg)))))
        (when (typep msg 'message:request)
            (remhash (threads:get-thread-id (bt:current-thread)) (thread-msgs state)))))


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

    (logger:info-msg (logger state) "Started state ~A" state))