(defpackage :alive/lsp/sem-analysis
    (:use :cl)
    (:export :to-sem-tokens)
    (:local-nicknames (:pos :alive/position)
                      (:sem-types :alive/lsp/types/sem-tokens)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:types :alive/types)))

(in-package :alive/lsp/sem-analysis)


(defclass open-form ()
    ((form-type :accessor form-type
                :initform nil
                :initarg :form-type)
     (expr-type :accessor expr-type
                :initform nil
                :initarg :expr-type)
     (lambda-list :accessor lambda-list
                  :initform nil
                  :initarg :lambda-list)
     (comment-out-p :accessor comment-out-p
                    :initform nil
                    :initarg :comment-out-p)))


(defmethod print-object ((obj open-form) out)
    (format out "{type: ~A; expr-type: ~A; lambda-list: ~A; comment-out: ~A}"
            (form-type obj)
            (expr-type obj)
            (lambda-list obj)
            (comment-out-p obj)))


(defclass analysis-state ()
    ((lex-tokens :accessor lex-tokens
                 :initform nil
                 :initarg :lex-tokens)
     (sem-tokens :accessor sem-tokens
                 :initform nil
                 :initarg :sem-tokens)
     (forced-type :accessor forced-type
                  :initform nil
                  :initarg :forced-type)
     (comment-next-p :accessor comment-next-p
                     :initform nil
                     :initarg :comment-next-p)
     (opens :accessor opens
            :initform (list (make-instance 'open-form :form-type :top-level-form))
            :initarg :opens)))


(defmethod print-object ((obj analysis-state) out)
    (format out "{lex ~A; sem ~A; comment-next ~A; opens ~A}"
            (lex-tokens obj)
            (reverse (sem-tokens obj))
            (comment-next-p obj)
            (opens obj)))


(defclass symbol-with-pkg ()
    ((pkg :accessor pkg
          :initform nil
          :initarg :pkg)
     (colons :accessor colons
             :initform nil
             :initarg :colons)
     (sym :accessor sym
          :initform nil
          :initarg :sym)))


(defmethod print-object ((obj symbol-with-pkg) out)
    (format out "{pkg: ~A; colons: ~A; sym: ~A}"
            (pkg obj)
            (colons obj)
            (sym obj)))


(defun peek-token (state &optional (ndx 0))
    (when (< ndx (length (lex-tokens state)))
          (elt (lex-tokens state) ndx)))


(defun eat-token (state)
    (setf (lex-tokens state) (cdr (lex-tokens state))))


(defun next-token (state)
    (let ((token (car (lex-tokens state))))

        (eat-token state)
        token))


(defun skip-ws (state)
    (when (and (peek-token state)
               (eq types:*ws* (token:get-type-value (peek-token state))))

          (eat-token state)))


(defun is-next-type (state target)
    (let ((peeked (peek-token state)))
        (and peeked
             (eq (token:get-type-value peeked) target))))


(defun add-sem-token (state token sem-type)
    (when token
          (loop :with new-type := (or (forced-type state)
                                      sem-type)
                :with start := (token:get-start token)
                :with end := (token:get-end token)

                :for line :from (pos:line start) :to (pos:line end)
                :for start-col := (if (eq line (pos:line start))
                                      (pos:col start)
                                      0)
                :for end-col := (if (eq line (pos:line end))
                                    (pos:col end)
                                    #xFFFFFFFF)
                :for new-token := (sem-types:create :line line
                                                    :start start-col
                                                    :end end-col
                                                    :token-type new-type) :do
                    (when new-type
                          (push new-token (sem-tokens state))))))


(defun is-number (text)
    (loop :with is-valid := T
          :with have-decimal := nil
          :with have-after-decimal := nil
          :with have-div := nil
          :with have-after-div := nil

          :for ch :across text :do
              (cond ((char= ch #\.) (if have-decimal
                                        (setf is-valid nil)
                                        (setf have-decimal T)))
                    ((char= ch #\/) (if have-div
                                        (setf is-valid nil)
                                        (setf have-div T)))
                    ((digit-char-p ch) (when (and is-valid
                                                  have-div)
                                             (setf have-after-div T))
                                       (when (and is-valid
                                                  have-decimal)
                                             (setf have-after-decimal T)))
                    (T (setf is-valid nil)))

          :finally (return (and is-valid
                                (eq have-div have-after-div)
                                (eq have-decimal have-after-decimal)))))


(defun convert-if-comment (state token-type)
    (if (or (comment-next-p state)
            (comment-out-p (car (opens state))))
        sem-types:*comment*
        token-type))


(defun get-symbol-type (sym &optional namespace)
    (cond ((is-number sym) sem-types:*number*)

          ((or (string= "NIL" (string-upcase sym))
               (string= "T" (string-upcase sym))) sem-types:*symbol*)

          ((symbols:macro-p sym namespace) sem-types:*macro*)

          ((symbols:function-p sym namespace) sem-types:*function*)

          ((symbols:callable-p sym namespace) sem-types:*keyword*)

          (() ())))


(defun update-symbol-types (state)
    (let ((token1 (peek-token state 0))
          (token2 (peek-token state 1))
          (token3 (peek-token state 2))
          (lambda-list nil))

        (cond ((and (token:is-type types:*symbol* token1)
                    (token:is-type types:*colons* token2)
                    (token:is-type types:*symbol* token3))
               (add-sem-token state token1 (convert-if-comment state sem-types:*namespace*))
               (add-sem-token state token2 (convert-if-comment state sem-types:*symbol*))
               (add-sem-token state token3 (convert-if-comment state (get-symbol-type (token:get-text token3)
                                                                                      (token:get-text token1))))
               (next-token state)
               (next-token state)

               (setf lambda-list (symbols:get-lambda-list (token:get-text token3)
                                                          (token:get-text token1))))

              ((and (token:is-type types:*symbol* token1)
                    (not (token:is-type types:*colons* token2)))
               (add-sem-token state token1 (convert-if-comment state (get-symbol-type (token:get-text token1))))
               (setf lambda-list (symbols:get-lambda-list (token:get-text token1)))))

        lambda-list))


(defun update-symbol-state (state lambda-list)
    (let ((open-form (car (opens state))))
        (if (eq :expr (form-type open-form))

            (if (not (expr-type open-form))
                (if lambda-list
                    (progn (setf (expr-type (car (opens state))) :fn-call)
                           (setf (lambda-list (car (opens state))) lambda-list))
                    (setf (expr-type (car (opens state))) :plain-list))

                (format T "SYMBOL EXPR HAS TYPE ~A~%" (expr-type open-form)))

            (format T "SYMBOL NOT AN EXPR ~A~%" (form-type open-form)))))


(defun process-symbol (state)
    (let ((lambda-list (update-symbol-types state)))

        (update-symbol-state state lambda-list)))


(defun process-token (state)
    (let ((token (peek-token state)))
        (cond ((token:is-type types:*ifdef-false* token)
               (add-sem-token state token sem-types:*comment*)
               (setf (comment-next-p state) T))

              ((or (token:is-type types:*quote* token)
                   (token:is-type types:*back-quote* token))
               (add-sem-token state token (convert-if-comment state sem-types:*symbol*))
               (push (make-instance 'open-form
                                    :form-type :quote
                                    :comment-out-p (or (comment-next-p state)
                                                       (comment-out-p (car (opens state)))))
                     (opens state))
               (setf (comment-next-p state) NIL))

              ((token:is-type types:*open-paren* token)
               (add-sem-token state token (convert-if-comment state sem-types:*parenthesis*))
               (push (make-instance 'open-form
                                    :form-type :expr
                                    :comment-out-p (or (comment-next-p state)
                                                       (comment-out-p (car (opens state)))))
                     (opens state))
               (setf (comment-next-p state) NIL))

              ((token:is-type types:*close-paren* token)
               (add-sem-token state token (convert-if-comment state sem-types:*parenthesis*))
               (setf (comment-next-p state) NIL)
               (when (eq :expr (form-type (car (opens state))))
                     (pop (opens state))))

              ((token:is-type types:*symbol* token)
               (process-symbol state)
               (setf (comment-next-p state) NIL))

              ((token:is-type types:*colons* token)
               (add-sem-token state token (convert-if-comment state sem-types:*symbol*))
               (when (token:is-type types:*symbol* (peek-token state 1))
                     (next-token state)
                     (add-sem-token state (peek-token state) (convert-if-comment state sem-types:*symbol*)))
               (setf (comment-next-p state) NIL))

              ((or (token:is-type types:*line-comment* token)
                   (token:is-type types:*block-comment* token))
               (add-sem-token state token sem-types:*comment*))

              ((token:is-type types:*string* token)
               (add-sem-token state token (convert-if-comment state sem-types:*string*)))

              ((token:is-type types:*macro* token)
               (add-sem-token state token (convert-if-comment state sem-types:*macro*)))

              ((token:is-type types:*ws* token)
               (when (eq :quote (form-type (car (opens state))))
                     (pop (opens state))))

              (T (format T "UNHANDLED TOKEN ~A~%" token))))

    (next-token state))


(defun to-sem-tokens (tokens)
    (format T "TO-SEM-TOKENS ~A~%" tokens)

    (loop :with state := (make-instance 'analysis-state :lex-tokens tokens)

          :while (lex-tokens state)
          :do (process-token state)

          :finally (return (reverse (sem-tokens state)))))
