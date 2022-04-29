(defpackage :alive/lsp/sem-analysis
    (:use :cl)
    (:export :to-sem-tokens)
    (:local-nicknames (:pos :alive/position)
                      (:sem-types :alive/lsp/types/sem-tokens)
                      (:packages :alive/packages)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:types :alive/types)))

(in-package :alive/lsp/sem-analysis)


(declaim (optimize (speed 3) (safety 0)))


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
         (expr-ndx :accessor expr-ndx
                   :initform 0
                   :initarg :expr-ndx)
         (comment-out-p :accessor comment-out-p
                        :initform nil
                        :initarg :comment-out-p)))


(defmethod print-object ((obj open-form) out)
    (declare (type stream out))
    (format out "{type: ~A; expr-type: ~A; lambda-list: ~A; expr-ndx: ~A; comment-out: ~A}"
        (form-type obj)
        (expr-type obj)
        (lambda-list obj)
        (expr-ndx obj)
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
         (cur-pkg :accessor cur-pkg
                  :initform nil
                  :initarg :cur-pkg)
         (opens :accessor opens
                :initform (list (make-instance 'open-form :form-type :top-level-form))
                :initarg :opens)))


(defmethod print-object ((obj analysis-state) out)
    (declare (type stream out))
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
    (declare (type stream out))
    (format out "{pkg: ~A; colons: ~A; sym: ~A}"
        (pkg obj)
        (colons obj)
        (sym obj)))


(defun peek-token (state)
    (car (lex-tokens state)))


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

            :for line :from (the fixnum (pos:line start)) :to (the fixnum (pos:line end))
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
    (declare (type simple-string text))
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


(defun lookup-symbol-type (sym &optional namespace)
    (cond ((is-number sym) sem-types:*number*)

        ((or (string= "NIL" (string-upcase sym))
             (string= "T" (string-upcase sym))) sem-types:*keyword*)

        ((symbols:macro-p sym namespace) sem-types:*macro*)

        ((symbols:function-p sym namespace) sem-types:*function*)

        ((symbols:callable-p sym namespace) sem-types:*keyword*)))


(defun get-symbol-type (state sym &optional namespace)
    (declare (type simple-string sym))

    (let ((open-form (car (opens state)))
          (*package* (if (cur-pkg state) (cur-pkg state) *package*)))
        (cond ((eq :lambda-list (expr-type open-form))
               (if (char= #\& (char sym 0))
                   sem-types:*keyword*
                   sem-types:*parameter*))

            ((eq :arg-init (expr-type open-form))
             (when (zerop (the fixnum (expr-ndx open-form)))
                 (incf (the fixnum (expr-ndx open-form)))
                 sem-types:*parameter*))

            (T (lookup-symbol-type sym namespace)))))


(defun update-symbol-types (state)
    (let ((token1 (peek-token state))
          (token2 (cadr (lex-tokens state)))
          (token3 (caddr (lex-tokens state)))
          (lambda-list nil)
          (*package* (if (cur-pkg state) (cur-pkg state) *package*)))

        (cond ((and (token:is-type types:*symbol* token1)
                   (token:is-type types:*colons* token2)
                   (token:is-type types:*symbol* token3))
               (add-sem-token state token1 (convert-if-comment state sem-types:*namespace*))
               (add-sem-token state token2 (convert-if-comment state sem-types:*symbol*))
               (add-sem-token state token3 (convert-if-comment state (lookup-symbol-type (token:get-text token3)
                                                                                         (token:get-text token1))))
               (next-token state)
               (next-token state)

               (setf lambda-list (symbols:get-lambda-list (token:get-text token3)
                                                          (token:get-text token1))))

            ((and (token:is-type types:*symbol* token1)
                 (not (token:is-type types:*colons* token2)))
             (add-sem-token state token1 (convert-if-comment state (get-symbol-type state (token:get-text token1))))
             (setf lambda-list (symbols:get-lambda-list (token:get-text token1)))))

        lambda-list))


(defun update-symbol-lambda-list (open-form)
    (when (lambda-list open-form)
        (let ((list-item (car (lambda-list open-form))))
            (declare (ignore list-item))
            (pop (lambda-list open-form)))))


(defun update-symbol-expr-state (open-form)
    (let* ((e-type (expr-type open-form)))

        (cond ((eq :fn-call e-type) (update-symbol-lambda-list open-form))

            ((or (eq :lambda-list e-type)
                 (eq :plain-list e-type)
                 (eq :arg-init e-type)
                 (eq :in-package e-type))
             nil)

            (T (format T "SYMBOL EXPR HAS TYPE ~A~%" e-type)))))


(defun update-symbol-fn-state (state lambda-list)
    (let ((token (peek-token state))
          (open-form (car (opens state))))
        (if (string= "in-package" (string-downcase (token:get-text token)))
            (progn (setf (cur-pkg state) nil)
                (setf (expr-type open-form) :in-package))
            (setf (expr-type open-form) :fn-call))
        (setf (lambda-list open-form) lambda-list)))


(defun update-symbol-state (state lambda-list)
    (let ((open-form (car (opens state))))
        (when (eq :expr (form-type open-form))

            (if (expr-type open-form)
                (update-symbol-expr-state open-form)
                (if lambda-list
                    (update-symbol-fn-state state lambda-list)
                    (setf (expr-type (car (opens state))) :plain-list))))))


(defun process-symbol (state)
    (let ((lambda-list (update-symbol-types state)))
        (update-symbol-state state lambda-list)))


(defun add-open-form (state form-type &optional expr-type)
    (push (make-instance 'open-form
              :form-type form-type
              :expr-type expr-type
              :comment-out-p (or (comment-next-p state)
                                 (comment-out-p (car (opens state)))))
          (opens state)))


(defun process-open-parens-fn-call (state open-form)
    (if (lambda-list open-form)
        (progn (let ((list-item (car (lambda-list open-form))))
                   (cond ((eq 'cons (type-of list-item))
                          (add-open-form state :expr :arg-init))

                       ((string= (the symbol list-item) "&REST")
                        (setf (lambda-list open-form) NIL)
                        (add-open-form state :expr))

                       ((string= (the symbol list-item) "LAMBDA-LIST")
                        (add-open-form state :expr :lambda-list))

                       (T (add-open-form state :expr))))

            (pop (lambda-list open-form)))
        (add-open-form state :expr)))


(defun process-open-parens-expr (state open-form)
    (let ((e-type (expr-type open-form)))
        (cond ((eq :fn-call e-type) (process-open-parens-fn-call state open-form))

            ((eq :lambda-list e-type) (add-open-form state :expr :arg-init))

            (T (add-open-form state :expr)))))


(defun process-open-parens (state)
    (let ((open-form (car (opens state))))
        (cond ((eq :expr (form-type open-form))
               (process-open-parens-expr state open-form))

            (T (add-open-form state :expr)))))


(defun process-token (state)
    (let ((token (peek-token state)))

        (cond ((token:is-type types:*ifdef-false* token)
               (add-sem-token state token sem-types:*comment*)
               (setf (comment-next-p state) T))

            ((token:is-type types:*ifdef-true* token)
             (add-sem-token state token (convert-if-comment state sem-types:*macro*)))

            ((or (token:is-type types:*quote* token)
                 (token:is-type types:*back-quote* token))
             (add-sem-token state token (convert-if-comment state sem-types:*keyword*))
             (add-open-form state :quote)
             (setf (comment-next-p state) NIL))

            ((token:is-type types:*open-paren* token)
             (add-sem-token state token (convert-if-comment state sem-types:*parenthesis*))
             (process-open-parens state)
             (setf (comment-next-p state) NIL))

            ((token:is-type types:*close-paren* token)
             (add-sem-token state token (convert-if-comment state sem-types:*parenthesis*))
             (setf (comment-next-p state) NIL)

             (loop :while (and (not (eq :top-level-form (form-type (car (opens state)))))
                              (not (eq :expr (form-type (car (opens state))))))
                 :do (pop (opens state)))

             (when (eq :expr (form-type (car (opens state))))
                 (pop (opens state))
                 (loop :while (eq :quote (form-type (car (opens state))))
                     :do (pop (opens state)))))

            ((token:is-type types:*symbol* token)
             (process-symbol state)
             (setf (comment-next-p state) NIL))

            ((token:is-type types:*colons* token)
             (add-sem-token state token (convert-if-comment state sem-types:*symbol*))
             (when (token:is-type types:*symbol* (cadr (lex-tokens state)))
                 (next-token state)
                 (when (and (not (cur-pkg state))
                           (opens state)
                           (eq :in-package (expr-type (car (opens state)))))
                     (setf (cur-pkg state)
                         (packages:for-string (format nil "~A~A"
                                                  (token:get-text token)
                                                  (token:get-text (peek-token state))))))
                 (add-sem-token state (peek-token state) (convert-if-comment state sem-types:*symbol*)))
             (setf (comment-next-p state) NIL))

            ((or (token:is-type types:*line-comment* token)
                 (token:is-type types:*block-comment* token))
             (add-sem-token state token sem-types:*comment*))

            ((or (token:is-type types:*comma* token)
                 (token:is-type types:*comma-at* token))
             (add-sem-token state token (convert-if-comment state sem-types:*keyword*)))

            ((token:is-type types:*string* token)
             (when (and (not (cur-pkg state))
                       (opens state)
                       (eq :in-package (expr-type (car (opens state)))))
                 (setf (cur-pkg state) (packages:for-string (token:get-text token))))
             (add-sem-token state token (convert-if-comment state sem-types:*string*)))

            ((token:is-type types:*macro* token)
             (when (and (not (cur-pkg state))
                       (opens state)
                       (eq :in-package (expr-type (car (opens state)))))
                 (setf (cur-pkg state) (packages:for-string (token:get-text token))))
             (add-sem-token state token (convert-if-comment state sem-types:*macro*)))

            ((token:is-type types:*ws* token)
             (when (eq :quote (form-type (car (opens state))))
                 (pop (opens state))))

            (T (format T "UNHANDLED TOKEN ~A~%" token))))

    (next-token state))


(defun to-sem-tokens (tokens)
    (loop :with state := (make-instance 'analysis-state
                             :lex-tokens tokens
                             :cur-pkg (packages:lookup "cl-user"))

        :while (lex-tokens state)
        :do (process-token state)

        :finally (return (reverse (sem-tokens state)))))