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
                      (:file :alive/file)
                      (:formatter :alive/format)
                      (:tokenizer :alive/parse/tokenizer)
                      (:analysis :alive/lsp/sem-analysis)
                      (:init :alive/lsp/message/initialize)
                      (:parse-tokens :alive/parse/forms)
                      (:load-file :alive/lsp/message/alive/load-file)
                      (:try-compile :alive/lsp/message/alive/try-compile)
                      (:stderr :alive/lsp/message/alive/stderr)
                      (:stdout :alive/lsp/message/alive/stdout)
                      (:top-form :alive/lsp/message/alive/top-form)
                      (:logger :alive/logger)
                      (:message :alive/lsp/message/abstract)
                      (:comps :alive/lsp/completions)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:errors :alive/lsp/errors)
                      (:text-doc :alive/lsp/types/text-doc)
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
     (listeners :accessor listeners
                :initform nil
                :initarg :listeners)
     (read-thread :accessor read-thread
                  :initform nil
                  :initarg :read-thread)))


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


(defmethod get-input-stream ((obj network-state))
    (usocket:socket-stream (conn obj)))


(defmethod get-output-stream ((obj network-state))
    (usocket:socket-stream (conn obj)))


(defmethod send-msg ((obj network-state) msg)
    (logger:trace-msg (logger obj) "<-- ~A~%" (json:encode-json-to-string msg))

    (write-string (packet:to-wire msg) (usocket:socket-stream (conn obj)))
    (force-output (usocket:socket-stream (conn obj))))


(defmethod handle-msg ((obj state) (msg init:request))
    (let* ((resp (init:create-response (message:id msg))))
        (send-msg obj resp)))


(defmethod handle-msg ((obj state) (msg init:initialized))
    (set-initialized obj T))


(defmethod handle-msg ((obj state) (msg did-open:did-open))
    (let ((uri (did-open:get-uri msg))
          (text (did-open:get-text msg)))

        (when text
              (set-file-text obj uri text))))


(defmethod handle-msg (state (msg did-change:did-change))
    (let ((uri (did-change:get-uri msg))
          (text (did-change:get-text msg)))

        (when text
              (set-file-text state uri text))))


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
           (forms (parse-tokens:from-stream (make-string-input-stream text))))

        (format T "forms ~A~%" pos)
        (send-msg state
                  (message:create-error-resp :id (message:id msg)
                                             :code errors:*internal-error*
                                             :message (format nil "Still working on it")))
        ; (loop :with start := nil
        ;       :with end := nil

        ;       :for kid :in kids :do
        ;           (destructuring-bind (kid-start kid-end body)

        ;                   kid

        ;               (declare (ignore body))

        ;               (when (<= kid-start offset (+ 1 kid-end))
        ;                     (setf start kid-start)
        ;                     (setf end kid-end)))

        ;       :finally (if (and start end)
        ;                    (send-msg state (top-form:create-response :id (message:id msg)
        ;                                                              :start start
        ;                                                              :end (+ 1 end)))
        ;                    (send-msg state (top-form:create-response :id (message:id msg)
        ;                                                              :start -1
        ;                                                              :end -1))))
    ))


(defmethod handle-msg (state (msg formatting:request))
    (let* ((params (message:params msg))
           (range (formatting:range params))
           (doc (formatting:text-document params))
           (uri (text-doc:uri doc))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (edits (formatter:range (make-string-input-stream text)
                                   range)))

        (send-msg state (formatting:create-response (message:id msg) edits))))


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
                     (logger:error-msg (logger state) "EOF caught, assuming socket is closed")
                     (stop state))

        (errors:unhandled-request (c)
                                  (logger:error-msg (logger state) "read-message: ~A" c)
                                  (send-msg state
                                            (message:create-error-resp :id (errors:id c)
                                                                       :code errors:*method-not-found*
                                                                       :message (format nil "Unhandled request: ~A" (errors:method-name c)))))

        (errors:server-error (c)
                             (logger:error-msg (logger state) "read-message: ~A" c)
                             (send-msg state
                                       (message:create-error-resp :id (errors:id c)
                                                                  :code errors:*internal-error*
                                                                  :message (format nil "Server error: ~A" (errors:message c)))))

        (T (c)
           (logger:error-msg (logger state) "read-message: ~A" c)
           (stop state))))


(defun read-messages (state)
    (loop :while (running state)
          :do (let ((msg (read-message state)))

                  (logger:debug-msg (logger state) "MSG ~A" (if msg T NIL))

                  (handler-case (when msg
                                      (logger:trace-msg (logger state) "--> ~A~%" (json:encode-json-to-string msg))
                                      (handle-msg state msg))
                      (error (c)
                             (logger:error-msg (logger state) "~A" c)
                             (send-msg state
                                       (message:create-error-resp :code errors:*internal-error*
                                                                  :message "Internal Server Error"
                                                                  :id (message:id msg))))))))


(defun start-read-thread (state)
    (let ((stdout *standard-output*))
        (setf (read-thread state)
              (bt:make-thread (lambda ()
                                  (let ((*standard-output* stdout))
                                      (read-messages state)))
                              :name "state Message Reader"))))


(defun start (state)
    (setf (running state) T)

    (start-read-thread state)

    (logger:info-msg (logger state) "Started state ~A" state))
