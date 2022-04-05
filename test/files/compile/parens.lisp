;;;;; -*- indent-tabs-mode: nil -*-
;;;
;;; swank-sbcl.lisp --- SLIME backend for SBCL.
;;;
;;; Created 2003, Daniel Barlow <dan@metacircles.com>
;;;
;;; This code has been placed in the Public Domain.  All warranties are
;;; disclaimed.

;;; Requires the SB-INTROSPECT contrib.

;;; Administrivia

(defpackage swank/sbcl
  (:use cl swank/backend swank/source-path-parser swank/source-file-cache))

(in-package swank/sbcl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-bsd-sockets)
  (require 'sb-introspect)
  (require 'sb-posix)
  (require 'sb-cltl2))

(declaim (optimize (debug 2)
                   (sb-c::insert-step-conditions 0)
                   (sb-c::insert-debug-catch 0)))

;;; backwards compability tests

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Generate a form suitable for testing for stepper support (0.9.17)
  ;; with #+.
  (defun sbcl-with-new-stepper-p ()
    (with-symbol 'enable-stepping 'sb-impl))
  ;; Ditto for weak hash-tables
  (defun sbcl-with-weak-hash-tables ()
    (with-symbol 'hash-table-weakness 'sb-ext))
  ;; And for xref support (1.0.1)
  (defun sbcl-with-xref-p ()
    (with-symbol 'who-calls 'sb-introspect))
  ;; ... for restart-frame support (1.0.2)
  (defun sbcl-with-restart-frame ()
    (with-symbol 'frame-has-debug-tag-p 'sb-debug))
  ;; ... for :setf :inverse info (1.1.17)
  (defun sbcl-with-setf-inverse-meta-info ()
    (boolean-to-feature-expression
     ;; going through FIND-SYMBOL since META-INFO was renamed from
     ;; TYPE-INFO in 1.2.10.
     (let ((sym (find-symbol "META-INFO" "SB-C")))
       (and sym
            (fboundp sym)
            (funcall sym :setf :inverse ()))))))

;;; swank-mop

(import-swank-mop-symbols :sb-mop '(:slot-definition-documentation))

(defun swank-mop:slot-definition-documentation (slot)
  (sb-pcl::documentation slot t))

;; stream support

(defimplementation gray-package-name ()
  "SB-GRAY")

;; Pretty printer calls this, apparently
(defmethod sb-gray:stream-line-length
    ((s sb-gray:fundamental-character-input-stream))
  nil)

;;; Connection info

(defimplementation lisp-implementation-type-name ()
  "sbcl")

;; Declare return type explicitly to shut up STYLE-WARNINGS about
;; %SAP-ALIEN in ENABLE-SIGIO-ON-FD below.
(declaim (ftype (function () (values (signed-byte 32) &optional)) getpid))
(defimplementation getpid ()
  (sb-posix:getpid))

;;; UTF8

(defimplementation string-to-utf8 (string)
  (sb-ext:string-to-octets string :external-format '(:utf8 :replacement
                                                     #+sb-unicode #\Replacement_Character
                                                     #-sb-unicode #\? )))

(defimplementation utf8-to-string (octets)
  (sb-ext:octets-to-string octets :external-format '(:utf8 :replacement
                                                     #+sb-unicode #\Replacement_Character
                                                     #-sb-unicode #\? )))

;;; TCP Server

(defimplementation preferred-communication-style ()
  (cond
    ;; fixme: when SBCL/win32 gains better select() support, remove
    ;; this.
    ((member :sb-thread *features*) :spawn)
    ((member :win32 *features*) nil)
    (t :fd-handler)))


(defun resolve-hostname (host)
  "Returns valid IPv4 or IPv6 address for the host."
  ;; get all IPv4 and IPv6 addresses as a list
  (let* ((host-ents (multiple-value-list (sb-bsd-sockets:get-host-by-name host)))
         ;; remove protocols for which we don't have an address
         (addresses (remove-if-not #'sb-bsd-sockets:host-ent-address host-ents)))
    ;; Return the first one or nil,
    ;; but actually, it shouln't return nil, because
    ;; get-host-by-name will signal NAME-SERVICE-ERROR condition
    ;; if there isn't any address for the host.
    (first addresses)))


(defimplementation create-socket (host port &key backlog)
  (let* ((host-ent (resolve-hostname host))
         (socket (make-instance (cond #+#.(swank/backend:with-symbol 'inet6-socket 'sb-bsd-sockets)
                                      ((eql (sb-bsd-sockets:host-ent-address-type host-ent) 10)
                                       'sb-bsd-sockets:inet6-socket)
                                      (t
                                       'sb-bsd-sockets:inet-socket))
                                :type :stream
                                :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket (sb-bsd-sockets:host-ent-address host-ent) port)

    (sb-bsd-sockets:socket-listen socket (or backlog 5))
    socket))

(defimplementation local-port (socket)
  (nth-value 1 (sb-bsd-sockets:socket-name socket)))

(defimplementation close-socket (socket)
  (sb-sys:invalidate-descriptor (socket-fd socket))
  (sb-bsd-sockets:socket-close socket))

(defimplementation accept-connection (socket &key
                                      external-format
                                      buffering timeout)
  (declare (ignore timeout))
  (make-socket-io-stream (accept socket) external-format
                         (ecase buffering
                           ((t :full) :full)
                           ((nil :none) :none)
                           ((:line) :line))))


;; The SIGIO stuff should probably be removed as it's unlikey that
;; anybody uses it.
#-win32
(progn
  (defimplementation install-sigint-handler (function)
    (sb-sys:enable-interrupt sb-unix:sigint
                             (lambda (&rest args)
                               (declare (ignore args))
                               (sb-sys:invoke-interruption
                                (lambda ()
                                  (sb-sys:with-interrupts
                                    (funcall function)))))))

  (defvar *sigio-handlers* '()
    "List of (key . fn) pairs to be called on SIGIO.")

  (defun sigio-handler (signal code scp)
    (declare (ignore signal code scp))
    (sb-sys:with-interrupts
      (mapc (lambda (handler)
              (funcall (the function (cdr handler))))
            *sigio-handlers*)))

  (defun set-sigio-handler ()
    (sb-sys:enable-interrupt sb-unix:sigio #'sigio-handler))

  (defun enable-sigio-on-fd (fd)
    (sb-posix::fcntl fd sb-posix::f-setfl sb-posix::o-async)
    (sb-posix::fcntl fd sb-posix::f-setown (getpid))
    (values))

  (defimplementation add-sigio-handler (socket fn)
    (set-sigio-handler)
    (let ((fd (socket-fd socket)))
      (enable-sigio-on-fd fd)
      (push (cons fd fn) *sigio-handlers*)))

  (defimplementation remove-sigio-handlers (socket)
    (let ((fd (socket-fd socket)))
      (setf *sigio-handlers* (delete fd *sigio-handlers* :key #'car))
      (sb-sys:invalidate-descriptor fd))
    (close socket)))


(defimplementation add-fd-handler (socket fun)
  (let ((fd (socket-fd socket))
        (handler nil))
    (labels ((add ()
               (setq handler (sb-sys:add-fd-handler fd :input #'run)))
             (run (fd)
               (sb-sys:remove-fd-handler handler) ; prevent recursion
               (unwind-protect
                    (funcall fun)
                 (when (sb-unix:unix-fstat fd) ; still open?
                   (add)))))
      (add))))

(defimplementation remove-fd-handlers (socket)
  (sb-sys:invalidate-descriptor (socket-fd socket)))

(defimplementation socket-fd (socket)
  (etypecase socket
    (fixnum socket)
    (sb-bsd-sockets:socket (sb-bsd-sockets:socket-file-descriptor socket))
    (file-stream (sb-sys:fd-stream-fd socket))))

(defimplementation command-line-args ()
  sb-ext:*posix-argv*)

(defimplementation dup (fd)
  (sb-posix:dup fd))

(defvar *wait-for-input-called*)

(defimplementation wait-for-input (streams &optional timeout)
  (assert (member timeout '(nil t)))
  (when (boundp '*wait-for-input-called*)
    (setq *wait-for-input-called* t))
  (let ((*wait-for-input-called* nil))
    (loop
      (let ((ready (remove-if-not #'input-ready-p streams)))
        (when ready (return ready)))
      (when (check-slime-interrupts)
        (return :interrupt))
      (when *wait-for-input-called*
        (return :interrupt))
      (when timeout
        (return nil))
      (sleep 0.1))))

(defun fd-stream-input-buffer-empty-p (stream)
  (let ((buffer (sb-impl::fd-stream-ibuf stream)))
    (or (not buffer)
        (= (sb-impl::buffer-head buffer)
           (sb-impl::buffer-tail buffer)))))

#-win32
(defun input-ready-p (stream)
  (or (not (fd-stream-input-buffer-empty-p stream))
      #+#.(swank/backend:with-symbol 'fd-stream-fd-type 'sb-impl)
      (eq :regular (sb-impl::fd-stream-fd-type stream))
      (not (sb-impl::sysread-may-block-p stream))))

#+win32
(progn
  (defun input-ready-p (stream)
    (or (not (fd-stream-input-buffer-empty-p stream))
        (handle-listen (sockint::fd->handle (sb-impl::fd-stream-fd stream)))))

  (sb-alien:define-alien-routine ("WSACreateEvent" wsa-create-event)
      sb-win32:handle)

  (sb-alien:define-alien-routine ("WSACloseEvent" wsa-close-event)
      sb-alien:int
    (event sb-win32:handle))

  (defconstant +fd-read+ #.(ash 1 0))
  (defconstant +fd-close+ #.(ash 1 5))

  (sb-alien:define-alien-routine ("WSAEventSelect" wsa-event-select)
      sb-alien:int
    (fd sb-alien:int)
    (handle sb-win32:handle)
    (mask sb-alien:long))

  (sb-alien:load-shared-object "kernel32.dll")
  (sb-alien:define-alien-routine ("WaitForSingleObjectEx"
                                  wait-for-single-object-ex)
      sb-alien:int
    (event sb-win32:handle)
    (milliseconds sb-alien:long)
    (alertable sb-alien:int))

  ;; see SB-WIN32:HANDLE-LISTEN
  (defun handle-listen (handle)
    (sb-alien:with-alien ((avail sb-win32:dword)
                          (buf (array char #.sb-win32::input-record-size)))
      (unless (zerop (sb-win32:peek-named-pipe handle nil 0 nil
                                               (sb-alien:alien-sap
                                                (sb-alien:addr avail))
                                               nil))
        (return-from handle-listen (plusp avail)))

      (unless (zerop (sb-win32:peek-console-input handle
                                                  (sb-alien:alien-sap buf)
                                                  sb-win32::input-record-size
                                                  (sb-alien:alien-sap
                                                   (sb-alien:addr avail))))
        (return-from handle-listen (plusp avail))))

    (let ((event (wsa-create-event)))
      (wsa-event-select handle event (logior +fd-read+ +fd-close+))
      (let ((val (wait-for-single-object-ex event 0 0)))
        (wsa-close-event event)
        (unless (= val -1)
          (return-from handle-listen (zerop val)))))

    nil)

  )

(defvar *external-format-to-coding-system*
  '((:iso-8859-1
     "latin-1" "latin-1-unix" "iso-latin-1-unix"
     "iso-8859-1" "iso-8859-1-unix")
    (:utf-8 "utf-8" "utf-8-unix")
    (:euc-jp "euc-jp" "euc-jp-unix")
    (:us-ascii "us-ascii" "us-ascii-unix")))

;; C.f. R.M.Kreuter in <20536.1219412774@progn.net> on sbcl-general,
;; 2008-08-22.
(defvar *physical-pathname-host* (pathname-host (user-homedir-pathname)))

(defimplementation filename-to-pathname (filename)
  (sb-ext:parse-native-namestring filename *physical-pathname-host*))

(defimplementation find-external-format (coding-system)
  (car (rassoc-if (lambda (x) (member coding-system x :test #'equal))
                  *external-format-to-coding-system*)))

(defimplementation set-default-directory (directory)
  (let ((directory (truename (merge-pathnames directory))))
    (sb-posix:chdir directory)
    (setf *default-pathname-defaults* directory)
    (default-directory)))

(defun make-socket-io-stream (socket external-format buffering)
  (let ((args `(:output t
                :input t
                :element-type ,(if external-format
                                   'character
                                   '(unsigned-byte 8))
                :buffering ,buffering
                ,@(cond ((and external-format (sb-int:featurep :sb-unicode))
                         `(:external-format ,external-format))
                        (t '()))
                :serve-events ,(eq :fd-handler swank:*communication-style*)
                  ;; SBCL < 1.0.42.43 doesn't support :SERVE-EVENTS
                  ;; argument.
                :allow-other-keys t)))
  (apply #'sb-bsd-sockets:socket-make-stream socket args)))

(defun accept (socket)
  "Like socket-accept, but retry on EAGAIN."
  (loop (handler-case
            (return (sb-bsd-sockets:socket-accept socket))
          (sb-bsd-sockets:interrupted-error ()))))


;;;; Support for SBCL syntax

;;; SBCL's source code is riddled with #! reader macros.  Also symbols
;;; containing `!' have special meaning.  We have to work long and
;;; hard to be able to read the source.  To deal with #! reader
;;; macros, we use a special readtable.  The special symbols are
;;; converted by a condition handler.

(defun feature-in-list-p (feature list)
  (etypecase feature
    (symbol (member feature list :test #'eq))
    (cons (flet ((subfeature-in-list-p (subfeature)
                   (feature-in-list-p subfeature list)))
            ;; Don't use ECASE since SBCL also has :host-feature,
            ;; don't need to handle it or anything else appearing in
            ;; the future or in erronous code.
            (case (first feature)
              (:or  (some  #'subfeature-in-list-p (rest feature)))
              (:and (every #'subfeature-in-list-p (rest feature)))
              (:not (destructuring-bind (e) (cdr feature)
                      (not (subfeature-in-list-p e)))))))))

(defun shebang-reader (stream sub-character infix-parameter)
  (declare (ignore sub-character))
  (when infix-parameter
    (error "illegal read syntax: #~D!" infix-parameter))
  (let ((next-char (read-char stream)))
    (unless (find next-char "+-")
      (error "illegal read syntax: #!~C" next-char))
    ;; When test is not satisfied
    ;; FIXME: clearer if order of NOT-P and (NOT NOT-P) were reversed? then
    ;; would become "unless test is satisfied"..
    (when (let* ((*package* (find-package "KEYWORD"))
                 (*read-suppress* nil)
                 (not-p (char= next-char #\-))
                 (feature (read stream)))
            (if (feature-in-list-p feature *features*)
		not-p
		(not not-p)))
      ;; Read (and discard) a form from input.
      (let ((*read-suppress* t))
	(read stream t nil t))))
 (values))

(defvar *shebang-readtable*
  (let ((*readtable* (copy-readtable nil)))
    (set-dispatch-macro-character #\# #\!
                                  (lambda (s c n) (shebang-reader s c n))
                                  *readtable*)
    *readtable*))

(defun shebang-readtable ()
  *shebang-readtable*)

(defun sbcl-package-p (package)
  (let ((name (package-name package)))
    (eql (mismatch "SB-" name) 3)))

(defun sbcl-source-file-p (filename)
  (when filename
    (loop for (nil pattern) in (logical-pathname-translations "SYS")
          thereis (pathname-match-p filename pattern))))

(defun guess-readtable-for-filename (filename)
  (if (sbcl-source-file-p filename)
      (shebang-readtable)
      *readtable*))

(defvar *debootstrap-packages* t)

(defun call-with-debootstrapping (fun)
  (handler-bind ((sb-int:bootstrap-package-not-found
                  #'sb-int:debootstrap-package))
    (funcall fun)))

(defmacro with-debootstrapping (&body body)
  `(call-with-debootstrapping (lambda () ,@body)))
