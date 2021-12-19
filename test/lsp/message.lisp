(defpackage :alive/test/lsp/message
    (:use :cl)
    (:export :run-all)
    (:local-nicknames (:parse :alive/lsp/parse))
)

(in-package :alive/test/lsp/message)


(defparameter *end-line* (format nil "~C~C" #\return #\linefeed))

; content ((JSONRPC . 2.0) (ID . 0) (METHOD . initialize)
;          (PARAMS (PROCESS-ID . 524519)
;           (CLIENT-INFO (NAME . Visual Studio Code) (VERSION . 1.62.3))
;           (LOCALE . en-us) (ROOT-PATH . /home/rich/work/lisp/remote)
;           (ROOT-URI . file:///home/rich/work/lisp/remote)
;           (CAPABILITIES
;            (WORKSPACE (APPLY-EDIT . T)
;             (WORKSPACE-EDIT (DOCUMENT-CHANGES . T)
;              (RESOURCE-OPERATIONS create rename delete)
;              (FAILURE-HANDLING . textOnlyTransactional)
;              (NORMALIZES-LINE-ENDINGS . T)
;              (CHANGE-ANNOTATION-SUPPORT (GROUPS-ON-LABEL . T)))
;             (DID-CHANGE-CONFIGURATION (DYNAMIC-REGISTRATION . T))
;             (DID-CHANGE-WATCHED-FILES (DYNAMIC-REGISTRATION . T))
;             (SYMBOL (DYNAMIC-REGISTRATION . T)
;              (SYMBOL-KIND
;               (VALUE-SET 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
;                22 23 24 25 26))
;              (TAG-SUPPORT (VALUE-SET 1)))
;             (CODE-LENS (REFRESH-SUPPORT . T))
;             (EXECUTE-COMMAND (DYNAMIC-REGISTRATION . T)) (CONFIGURATION . T)
;             (WORKSPACE-FOLDERS . T) (SEMANTIC-TOKENS (REFRESH-SUPPORT . T))
;             (FILE-OPERATIONS (DYNAMIC-REGISTRATION . T) (DID-CREATE . T)
;              (DID-RENAME . T) (DID-DELETE . T) (WILL-CREATE . T)
;              (WILL-RENAME . T) (WILL-DELETE . T)))
;            (TEXT-DOCUMENT
;             (PUBLISH-DIAGNOSTICS (RELATED-INFORMATION . T) (VERSION-SUPPORT)
;              (TAG-SUPPORT (VALUE-SET 1 2)) (CODE-DESCRIPTION-SUPPORT . T)
;              (DATA-SUPPORT . T))
;             (SYNCHRONIZATION (DYNAMIC-REGISTRATION . T) (WILL-SAVE . T)
;              (WILL-SAVE-WAIT-UNTIL . T) (DID-SAVE . T))
;             (COMPLETION (DYNAMIC-REGISTRATION . T) (CONTEXT-SUPPORT . T)
;              (COMPLETION-ITEM (SNIPPET-SUPPORT . T)
;               (COMMIT-CHARACTERS-SUPPORT . T)
;               (DOCUMENTATION-FORMAT markdown plaintext)
;               (DEPRECATED-SUPPORT . T) (PRESELECT-SUPPORT . T)
;               (TAG-SUPPORT (VALUE-SET 1)) (INSERT-REPLACE-SUPPORT . T)
;               (RESOLVE-SUPPORT
;                (PROPERTIES documentation detail additionalTextEdits))
;               (INSERT-TEXT-MODE-SUPPORT (VALUE-SET 1 2)))
;              (COMPLETION-ITEM-KIND
;               (VALUE-SET 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
;                22 23 24 25)))
;             (HOVER (DYNAMIC-REGISTRATION . T)
;              (CONTENT-FORMAT markdown plaintext))
;             (SIGNATURE-HELP (DYNAMIC-REGISTRATION . T)
;              (SIGNATURE-INFORMATION (DOCUMENTATION-FORMAT markdown plaintext)
;               (PARAMETER-INFORMATION (LABEL-OFFSET-SUPPORT . T))
;               (ACTIVE-PARAMETER-SUPPORT . T))
;              (CONTEXT-SUPPORT . T))
;             (DEFINITION (DYNAMIC-REGISTRATION . T) (LINK-SUPPORT . T))
;             (REFERENCES (DYNAMIC-REGISTRATION . T))
;             (DOCUMENT-HIGHLIGHT (DYNAMIC-REGISTRATION . T))
;             (DOCUMENT-SYMBOL (DYNAMIC-REGISTRATION . T)
;              (SYMBOL-KIND
;               (VALUE-SET 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
;                22 23 24 25 26))
;              (HIERARCHICAL-DOCUMENT-SYMBOL-SUPPORT . T)
;              (TAG-SUPPORT (VALUE-SET 1)) (LABEL-SUPPORT . T))
;             (CODE-ACTION (DYNAMIC-REGISTRATION . T) (IS-PREFERRED-SUPPORT . T)
;              (DISABLED-SUPPORT . T) (DATA-SUPPORT . T)
;              (RESOLVE-SUPPORT (PROPERTIES edit))
;              (CODE-ACTION-LITERAL-SUPPORT
;               (CODE-ACTION-KIND
;                (VALUE-SET  quickfix refactor refactor.extract refactor.inline
;                 refactor.rewrite source source.organizeImports)))
;              (HONORS-CHANGE-ANNOTATIONS))
;             (CODE-LENS (DYNAMIC-REGISTRATION . T))
;             (FORMATTING (DYNAMIC-REGISTRATION . T))
;             (RANGE-FORMATTING (DYNAMIC-REGISTRATION . T))
;             (ON-TYPE-FORMATTING (DYNAMIC-REGISTRATION . T))
;             (RENAME (DYNAMIC-REGISTRATION . T) (PREPARE-SUPPORT . T)
;              (PREPARE-SUPPORT-DEFAULT-BEHAVIOR . 1)
;              (HONORS-CHANGE-ANNOTATIONS . T))
;             (DOCUMENT-LINK (DYNAMIC-REGISTRATION . T) (TOOLTIP-SUPPORT . T))
;             (TYPE-DEFINITION (DYNAMIC-REGISTRATION . T) (LINK-SUPPORT . T))
;             (IMPLEMENTATION (DYNAMIC-REGISTRATION . T) (LINK-SUPPORT . T))
;             (COLOR-PROVIDER (DYNAMIC-REGISTRATION . T))
;             (FOLDING-RANGE (DYNAMIC-REGISTRATION . T) (RANGE-LIMIT . 5000)
;              (LINE-FOLDING-ONLY . T))
;             (DECLARATION (DYNAMIC-REGISTRATION . T) (LINK-SUPPORT . T))
;             (SELECTION-RANGE (DYNAMIC-REGISTRATION . T))
;             (CALL-HIERARCHY (DYNAMIC-REGISTRATION . T))
;             (SEMANTIC-TOKENS (DYNAMIC-REGISTRATION . T)
;              (TOKEN-TYPES namespace type class enum interface struct
;               typeParameter parameter variable property enumMember event
;               function method macro keyword modifier comment string number
;               regexp operator)
;              (TOKEN-MODIFIERS declaration definition readonly static deprecated
;               abstract async modification documentation defaultLibrary)
;              (FORMATS relative) (REQUESTS (RANGE . T) (FULL (DELTA . T)))
;              (MULTILINE-TOKEN-SUPPORT) (OVERLAPPING-TOKEN-SUPPORT))
;             (LINKED-EDITING-RANGE (DYNAMIC-REGISTRATION . T)))
;            (WINDOW
;             (SHOW-MESSAGE
;              (MESSAGE-ACTION-ITEM (ADDITIONAL-PROPERTIES-SUPPORT . T)))
;             (SHOW-DOCUMENT (SUPPORT . T)) (WORK-DONE-PROGRESS . T))
;            (GENERAL
;             (REGULAR-EXPRESSIONS (ENGINE . ECMAScript) (VERSION . ES2020))
;             (MARKDOWN (PARSER . marked) (VERSION . 1.1.0))))
;           (TRACE . off)
;           (WORKSPACE-FOLDERS
;            ((URI . file:///home/rich/work/lisp/remote) (NAME . remote)))))


(defun create-content ()
    (with-output-to-string (str)
        (format str "{~A" *end-line*)
        (format str "  \"jsonrpc\": 2.0,~A" *end-line*)
        (format str "  \"id\": 0,~A" *end-line*)
        (format str "  \"method\": \"initialize\",~A" *end-line*)
        (format str "  \"params\": {~A" *end-line*)
        (format str "    \"client-info\": {~A" *end-line*)
        (format str "      \"name\": \"Visual Studio Code\",~A" *end-line*)
        (format str "      \"version\": \"1.62.3\"~A" *end-line*)
        (format str "    }~A" *end-line*)
        (format str "  }~A" *end-line*)
        (format str "}~A" *end-line*)
    ))


(defun create-msg ()
    (with-output-to-string (str)
        (let ((content (create-content)))
            (format str "Content-Length: ~A~A" (length content) *end-line*)
            (format str "~A" *end-line*)
            (format str "~A" content)
        )))


(defun parse-msg ()
    (let ((msg (create-msg)))
        (parse:from-stream (make-string-input-stream msg))
    ))


(defun run-all ()
    (parse-msg)
)
