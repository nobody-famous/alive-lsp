(defpackage :alive/session
    (:use :cl)
    (:export :add-listener
             :create
             :listener
             :start
             :stop)
    (:local-nicknames (:asdf :alive/asdf)
                      (:eval :alive/eval)
                      (:debugger :alive/debugger)
                      (:inspector :alive/inspector)
                      (:file :alive/file)
                      (:pos :alive/position)
                      (:packages :alive/packages)
                      (:threads :alive/threads)
                      (:formatter :alive/format)
                      (:logger :alive/logger)

                      (:analysis :alive/lsp/sem-analysis)
                      (:comps :alive/lsp/completions)
                      (:packet :alive/lsp/packet)
                      (:parse :alive/lsp/parse)
                      (:errors :alive/lsp/errors)

                      (:tokenizer :alive/parse/tokenizer)
                      (:form :alive/parse/form)
                      (:forms :alive/parse/forms)

                      (:config-item :alive/lsp/types/config-item)
                      (:fmt-opts :alive/lsp/types/format-options)
                      (:restart-info :alive/lsp/types/restart-info)

                      (:resp :alive/lsp/message/response)
                      (:req :alive/lsp/message/request)
                      (:notification :alive/lsp/message/notification)
                      (:message :alive/lsp/message/abstract)
                      (:fmt-utils :alive/lsp/message/format-utils)))

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
        (when (and (hash-table-p msg)
                   (gethash "jsonrpc" msg))
              (write-sequence (flexi-streams:string-to-octets (packet:to-wire msg)) (usocket:socket-stream (conn obj)))
              (force-output (usocket:socket-stream (conn obj))))))


(defun save-thread-msg (state id)
    (let* ((table (thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (threads:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((lock state))
            (setf (gethash thread-id table) id))))


(defun rem-thread-msg (state)
    (let* ((table (thread-msgs state))
           (cur-thread (bt:current-thread))
           (thread-id (threads:get-thread-id cur-thread)))

        (bt:with-recursive-lock-held ((lock state))
            (remhash thread-id table))))


(defun get-frame-text-stream (state file)
    (let* ((file-url (format NIL "file://~A" file))
           (files (files state))
           (text (gethash file-url files)))

        (when text
              (make-string-input-stream text))))


(defun frame-to-wire (state frame)
    (let* ((obj (make-hash-table :test #'equalp))
           (file (gethash "file" frame))
           (fn-name (gethash "function" frame))
           (pos (debugger:get-frame-loc (get-frame-text-stream state file)
                                        frame)))

        (setf (gethash "function" obj) fn-name)
        (setf (gethash "file" obj) file)
        (setf (gethash "position" obj) pos)

        obj))


(defun wait-for-debug (state err restarts frames)
    (let ((send-id (next-send-id state))
          (cond-var (bt:make-condition-variable))
          (restart-ndx nil))

        (setf (gethash send-id (sent-msg-callbacks state))
            (lambda (debug-resp)
                (unwind-protect
                        (cond ((assoc :error debug-resp)
                                  (logger:msg logger:*error* "Debugger Error ~A" debug-resp))

                              ((assoc :result debug-resp)
                                  (let* ((result (cdr (assoc :result debug-resp))))
                                      (when result
                                            (setf restart-ndx (cdr (assoc :index result)))))))
                    (bt:condition-notify cond-var))))

        (send-msg state (req:debugger send-id
                                      :message (princ-to-string err)
                                      :restarts restarts
                                      :stack-trace (mapcar (lambda (frame)
                                                               (frame-to-wire state frame))
                                                           frames)))

        (bt:with-recursive-lock-held ((lock state))
            (bt:condition-wait cond-var (lock state)))

        restart-ndx))


(defun start-debugger (state err frames)
    (let* ((restarts (compute-restarts err))
           (ndx (wait-for-debug state
                                err
                                (mapcar (lambda (item)
                                            (restart-info:create-item :name (restart-name item)
                                                                      :description (princ-to-string item)))
                                        restarts)
                                frames)))

        (when (and ndx
                   (<= 0 ndx (- (length restarts) 1)))
              (invoke-restart-interactively (elt restarts ndx)))))


(defun run-in-thread (state msg fn)
    (let ((stdout *standard-output*))
        (bt:make-thread (lambda ()
                            (unwind-protect
                                    (let ((*standard-output* stdout))
                                        (save-thread-msg state (cdr (assoc :id msg)))

                                        (block handler
                                            (handler-bind ((sb-impl::step-form-condition (lambda (err)
                                                                                             (start-debugger state err (alive/frames:list-step-frames))
                                                                                             (return-from handler)))
                                                           (error (lambda (err)
                                                                      (start-debugger state err (alive/frames:list-debug-frames))
                                                                      (return-from handler))))
                                                (funcall fn))))
                                (rem-thread-msg state)))

                        :name (next-thread-name state (if (assoc :id msg)
                                                          (cdr (assoc :method msg))
                                                          "response")))))


(defun cancel-thread (state thread-id msg-id)
    (when msg-id
          (send-msg state
                    (message:create-error msg-id
                                          :code errors:*request-cancelled*
                                          :message (format nil "Request ~A canceled" msg-id))))

    (when thread-id
          (threads:kill thread-id)))


(defun wait-for-input (state)
    (let ((send-id (next-send-id state))
          (cond-var (bt:make-condition-variable))
          (text nil))

        (setf (gethash send-id (sent-msg-callbacks state))
            (lambda (input-resp)
                (unwind-protect
                        (cond ((assoc :error input-resp)
                                  (logger:msg logger:*error* "Input Error ~A" input-resp))

                              ((assoc :result input-resp)
                                  (let* ((result (cdr (assoc :result input-resp)))
                                         (text-result (if result
                                                          (cdr (assoc :text result))
                                                          "")))
                                      (setf text text-result))))
                    (bt:condition-notify cond-var))))

        (send-msg state (message:create-request send-id "$/alive/userInput"))

        (bt:with-recursive-lock-held ((lock state))
            (bt:condition-wait cond-var (lock state)))

        text))


(defun try-inspect (state id text pkg-name)
    (let ((result (eval:from-string text
                                    :pkg-name pkg-name
                                    :stdin-fn (lambda ()
                                                  (wait-for-input state))
                                    :stdout-fn (lambda (data)
                                                   (send-msg state (notification:stdout data)))
                                    :stderr-fn (lambda (data)
                                                   (send-msg state (notification:stderr data))))))

        (add-inspector state
                       :id id
                       :inspector (inspector:create :text text
                                                    :pkg pkg-name
                                                    :result result))

        (inspector:to-result result)))


(defun process-inspect (state msg)
    (let ((id (cdr (assoc :id msg))))

        (handler-case
                (let* ((params (cdr (assoc :params msg)))
                       (pkg-name (cdr (assoc :package params)))
                       (text (cdr (assoc :text params)))
                       (insp-id (next-inspector-id state))
                       (* (elt (history state) 0))
                       (** (elt (history state) 1))
                       (*** (elt (history state) 2)))

                    (let ((result (try-inspect state insp-id text pkg-name)))
                        (send-msg state
                                  (resp:do-inspect id
                                                   :insp-id insp-id
                                                   :result result))))
            (T (c)
               (send-msg state
                         (message:create-error id
                                               :code errors:*internal-error*
                                               :message (princ-to-string c)))))))


(defun handle-inspect (state msg)
    (run-in-thread state msg (lambda ()
                                 (process-inspect state msg))))


(defun process-inspect-sym (state msg)
    (let ((id (cdr (assoc :id msg))))

        (handler-case
                (let* ((params (cdr (assoc :params msg)))
                       (pkg-name (cdr (assoc :package params)))
                       (name (cdr (assoc :symbol params)))
                       (insp-id (next-inspector-id state))
                       (sym (alive/symbols:lookup name pkg-name))
                       (result (inspector:to-result sym)))

                    (add-inspector state
                                   :id insp-id
                                   :inspector (inspector:create :text name
                                                                :pkg pkg-name
                                                                :result result))

                    (send-msg state (resp:do-inspect id
                                                     :insp-id insp-id
                                                     :result result)))
            (T (c)
               (send-msg state (message:create-error id
                                                     :code errors:*internal-error*
                                                     :message (princ-to-string c)))))))


(defun handle-inspect-sym (state msg)
    (run-in-thread state msg (lambda ()
                                 (process-inspect-sym state msg))))


(defun handle-inspect-close (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (insp-id (cdr (assoc :id params))))

        (rem-inspector state :id insp-id)
        (message:create-response id :result-value T)))


(defun stop (state)
    (when (logger:has-level logger:*info*)
          (logger:msg logger:*info* "Stopping state ~A" state))

    (setf (running state) NIL)

    (destroy state)

    (loop :for listener :in (listeners state)
          :do (when (on-done listener)
                    (funcall (on-done listener)))))


(defun read-message (state)
    (parse:from-stream (get-input-stream state)))


(defun next-thread-name (state method-name)
    (let ((name (format nil "~A - ~A" (thread-name-id state) method-name)))
        (incf (thread-name-id state))
        name))


(defun handle-init (state msg)
    (declare (ignore state))

    (resp:initialize (cdr (assoc :id msg))))


(defun handle-initialized (state msg)
    (declare (ignore msg))

    (set-initialized state T))


(defun handle-load-file (state msg)
    (run-in-thread state msg (lambda ()
                                 (let* ((id (cdr (assoc :id msg)))
                                        (params (cdr (assoc :params msg)))
                                        (path (cdr (assoc :path params)))
                                        (msgs (file:do-load path
                                                            :stdout-fn (lambda (data)
                                                                           (when (assoc :show-stdout params)
                                                                                 (send-msg state (notification:stdout data))))
                                                            :stderr-fn (lambda (data)
                                                                           (when (assoc :show-stderr params)
                                                                                 (send-msg state (notification:stderr data)))))))

                                     (resp:load-file id msgs)))))


(defun handle-completion (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (items (comps:simple :text text :pos pos)))

        (resp:completion id
                         :items items)))


(defun handle-symbol (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (result (alive/lsp/symbol:for-pos :text text :pos pos)))

        (resp:get-symbol id
                         :value result)))


(defun handle-hover (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (hov-text (alive/lsp/hover:get-text :text text :pos pos))
           (result (if hov-text hov-text "")))

        (resp:hover id
                    :value result)))


(defun handle-top-form (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
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

              :finally (return (resp:top-form id
                                              :start start
                                              :end end)))))


(defun handle-format-msg (state options msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (range (cdr (assoc :range params)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (edits (formatter:range (make-string-input-stream text)
                                   range
                                   options)))

        (message:create-response id
                                 :result-value (fmt-utils:to-text-edits edits))))


(defun handle-formatting (state msg)
    (let ((send-id (next-send-id state)))
        (setf (gethash send-id (sent-msg-callbacks state))
            (lambda (config-resp)
                (let ((opts (cdr (assoc :result config-resp))))
                    (handle-format-msg state (first opts) msg))))

        (send-msg state (req:config send-id
                                    :items (list (config-item:create-item :section "alive.format"))))))


(defun handle-on-type (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (opts (cdr (assoc :options params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (edits (formatter:on-type (make-string-input-stream text)
                                     :options (fmt-opts:convert opts)
                                     :pos pos))
           (value (if edits
                      (fmt-utils:to-text-edits edits)
                      (make-array 0))))

        (message:create-response id :result-value value)))


(defun handle-list-threads (state msg)
    (bt:with-recursive-lock-held ((lock state))
        (let ((threads (remove-if (lambda (thread)
                                      (eq (cdr (assoc :id thread)) (threads:get-thread-id (bt:current-thread))))
                               (threads:list-all))))

            (resp:list-items (cdr (assoc :id msg))
                             "threads"
                             threads))))


(defun handle-kill-thread (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (thread-id (cdr (assoc :id params))))

        (handler-case
                (progn (cancel-thread state
                                      thread-id
                                      (gethash thread-id
                                               (thread-msgs state)))
                       (message:create-response id :result-value T))

            (threads:thread-not-found (c)
                                      (message:create-error id
                                                            :code errors:*request-failed*
                                                            :message (format nil "Thread ~A not found" (threads:id c)))))))


(defun handle-list-pkgs (state msg)
    (declare (ignore state))

    (resp:list-items (cdr (assoc :id msg))
                     "packages"
                     (packages:list-all)))


(defun handle-unexport (state msg)
    (declare (ignore state))

    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (sym-name (cdr (assoc :symbol params)))
           (pkg-name (cdr (assoc :package params))))

        (packages:unexport-symbol pkg-name sym-name)
        (message:create-response id :result-value T)))


(defun handle-did-change (state msg)
    (let* ((params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (changes (cdr (assoc :content-changes params)))
           (text (cdr (assoc :text (first changes)))))

        (when text
              (bt:with-recursive-lock-held ((lock state))
                  (set-file-text state uri text)
                  nil))))


(defun handle-did-open (state msg)
    (let* ((params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (text (cdr (assoc :text doc))))

        (when text
              (bt:with-recursive-lock-held ((lock state))
                  (set-file-text state uri text)
                  nil))))


(defun process-eval (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :package params)))
           (text (cdr (assoc :text params)))
           (* (elt (history state) 0))
           (** (elt (history state) 1))
           (*** (elt (history state) 2))
           (result (eval:from-string text
                                     :pkg-name pkg-name
                                     :stdin-fn (lambda ()
                                                   (wait-for-input state))
                                     :stdout-fn (lambda (data)
                                                    (send-msg state (notification:stdout data)))
                                     :stderr-fn (lambda (data)
                                                    (send-msg state (notification:stderr data))))))

        (when (cdr (assoc :store-result params))
              (add-history state result))

        (send-msg state (resp:do-eval id
                                      (format nil "~A" result)))))


(defun handle-eval (state msg)
    (run-in-thread state msg (lambda ()
                                 (process-eval state msg))))


(defun handle-get-pkg (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (pos (cdr (assoc :position params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (pkg (packages:for-pos text pos)))

        (resp:get-pkg id
                      :pkg-name pkg)))


(defun handle-remove-pkg (state msg)
    (declare (ignore state))

    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (pkg-name (cdr (assoc :name params))))

        (packages:do-remove pkg-name)
        (message:create-response id :result-value T)))


(defun handle-list-asdf (state msg)
    (declare (ignore state))

    (resp:list-items (cdr (assoc :id msg))
                     "systems"
                     (asdf:list-systems)))


(defun handle-sem-tokens (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (doc (cdr (assoc :text-document params)))
           (uri (cdr (assoc :uri doc)))
           (file-text (get-file-text state uri))
           (text (if file-text file-text ""))
           (sem-tokens (analysis:to-sem-tokens
                           (tokenizer:from-stream
                               (make-string-input-stream text)))))

        (resp:sem-tokens id sem-tokens)))


(defun handle-try-compile (state msg)
    (declare (ignore state))
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (path (cdr (assoc :path params)))
           (msgs (file:try-compile path)))

        (resp:try-compile id msgs)))


(defun handle-load-asdf (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (params (cdr (assoc :params msg)))
           (name (cdr (assoc :name params))))

        (run-in-thread state msg (lambda ()
                                     (asdf:load-system :name name
                                                       :stdout-fn (lambda (data)
                                                                      (send-msg state (notification:stdout data)))
                                                       :stderr-fn (lambda (data)
                                                                      (send-msg state (notification:stderr data))))
                                     (send-msg state (message:create-response id :result-value T))))))


(defun ignore-msg (state msg)
    (declare (ignore state msg))
    nil)


(defparameter *handlers* (list (cons "initialize" 'handle-init)
                               (cons "initialized" 'handle-initialized)

                               (cons "textDocument/completion" 'handle-completion)
                               (cons "textDocument/didChange" 'handle-did-change)
                               (cons "textDocument/didClose" 'handle-did-change)
                               (cons "textDocument/didOpen" 'handle-did-open)
                               (cons "textDocument/didSave" 'ignore-msg)
                               (cons "textDocument/hover" 'handle-hover)
                               (cons "textDocument/onTypeFormatting" 'handle-on-type)
                               (cons "textDocument/rangeFormatting" 'handle-formatting)
                               (cons "textDocument/semanticTokens/full" 'handle-sem-tokens)

                               (cons "$/setTrace" 'ignore-msg)

                               (cons "$/alive/eval" 'handle-eval)
                               (cons "$/alive/getPackageForPosition" 'handle-get-pkg)
                               (cons "$/alive/inspect" 'handle-inspect)
                               (cons "$/alive/inspectClose" 'handle-inspect-close)
                               (cons "$/alive/inspectSymbol" 'handle-inspect-sym)
                               (cons "$/alive/killThread" 'handle-kill-thread)
                               (cons "$/alive/listAsdfSystems" 'handle-list-asdf)
                               (cons "$/alive/listPackages" 'handle-list-pkgs)
                               (cons "$/alive/listThreads" 'handle-list-threads)
                               (cons "$/alive/loadFile" 'handle-load-file)
                               (cons "$/alive/loadAsdfSystem" 'handle-load-asdf)
                               (cons "$/alive/removePackage" 'handle-remove-pkg)
                               (cons "$/alive/symbol" 'handle-symbol)
                               (cons "$/alive/topFormBounds" 'handle-top-form)
                               (cons "$/alive/tryCompile" 'handle-try-compile)
                               (cons "$/alive/unexportSymbol" 'handle-unexport)))


(defun handle-request (state msg)
    (let* ((id (cdr (assoc :id msg)))
           (method-name (cdr (assoc :method msg)))
           (handler (cdr (assoc method-name *handlers* :test #'string=))))

        (if handler
            (funcall handler state msg)
            (let ((error-msg (format nil "No handler for ~A" method-name)))
                (logger:msg logger:*error* error-msg)
                (when id (message:create-error id
                                               :code errors:*request-failed*
                                               :message error-msg))))))


(defun handle-response (state msg)
    (let* ((msg-id (cdr (assoc :id msg)))
           (cb (gethash msg-id (sent-msg-callbacks state))))

        (if cb
            (funcall cb msg)
            (message:create-error msg-id
                                  :code errors:*request-failed*
                                  :message (format nil "No callback for request: ~A" msg-id)))))


(defun handle-msg (state msg)
    (let* ((id (cdr (assoc :id msg))))

        (cond ((assoc :method msg) (handle-request state msg))
              ((assoc :result msg) (handle-response state msg))
              (T (message:create-error id
                                       :code errors:*request-failed*
                                       :message (format nil "No handler for message"))))))


(defun process-msg (state msg)
    (let ((id (cdr (assoc :id msg))))

        (unwind-protect
                (handler-case

                        (progn (when id
                                     (save-thread-msg state id))

                               (when (logger:has-level logger:*trace*)
                                     (logger:msg logger:*trace* "--> ~A~%" (json:encode-json-to-string msg)))

                               (handle-msg state msg))

                    (error (c)
                        (logger:msg logger:*error* "Message Handler: ~A" c)
                        (message:create-error id
                                              :code errors:*internal-error*
                                              :message (princ-to-string c))))
            (when id
                  (rem-thread-msg state)))))


(defun get-next-response (state)
    (handler-case
            (let ((msg (read-message state)))
                (when msg
                      (process-msg state msg)))

        (errors:unhandled-request (c)
                                  (logger:msg logger:*error* "read-message: ~A" c)
                                  (when (errors:id c)
                                        (message:create-error (errors:id c)
                                                              :code errors:*method-not-found*
                                                              :message (format nil "Unhandled request: ~A" (errors:method-name c)))))

        (errors:server-error (c)
                             (logger:msg logger:*error* "read-message: ~A" c)
                             (when (errors:id c)
                                   (message:create-error (errors:id c)
                                                         :code errors:*internal-error*
                                                         :message (format nil "Server error: ~A" (errors:message c)))))

        (end-of-file (c)
                     (declare (ignore c))
                     (stop state))

        (T (c)
           (logger:msg logger:*error* "read-message: ~A" c)
           (stop state))))


(defun read-messages (state)
    (loop :while (running state)
          :do (let ((resp (get-next-response state)))
                  (when resp
                        (send-msg state resp)))))


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