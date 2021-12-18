(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:alive-msg :alive/lsp/message))
)

(in-package :alive/test/lsp/message)


(defparameter *end-line* (format nil "~C~C" #\return #\linefeed))


(defun create-content ()
    (with-output-to-string (str)
        (format str "{~A" *end-line*)
        (format str "  \"jsonrpc\": 2.0,~A" *end-line*)
        (format str "  \"id\": 0,~A" *end-line*)
        (format str "  \"method\": \"initialize\",~A" *end-line*)
        (format str "  \"params\": {~A" *end-line*)
        (format str "    \"client-info\": \"Visual Studio Code\",~A" *end-line*)
        (format str "    \"version\": \"1.62.3\"~A" *end-line*)
        (format str "  }~A" *end-line*)
        (format str "}~A" *end-line*)
    ))


(defun create-msg ()
    (with-output-to-string (str)
        (let ((content (create-content)))
            (format T "~A~%" content)
            (format str "Content-Length: ~A~A" (length content) *end-line*)
            (format str "~A" *end-line*)
            (format str "~A" content)
        )))


(defun parse-msg ()
    (let ((msg (create-msg)))
        (alive-msg:parse (make-string-input-stream msg))
    ))


(defun run-all ()
    (parse-msg)
)
