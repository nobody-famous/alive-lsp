(defpackage :alive/lsp/completions
    (:use :cl)
    (:export :simple

             :*kind-text*
             :*kind-method*
             :*kind-function*
             :*kind-constructor*
             :*kind-field*
             :*kind-variable*
             :*kind-class*
             :*kind-interface*
             :*kind-module*
             :*kind-property*
             :*kind-unit*
             :*kind-value*
             :*kind-enum*
             :*kind-keyword*
             :*kind-snippet*
             :*kind-color*
             :*kind-file*
             :*kind-reference*
             :*kind-folder*
             :*kind-enum-member*
             :*kind-constant*
             :*kind-struct*
             :*kind-event*
             :*kind-operator*
             :*kind-type-parameter*)
    (:local-nicknames (:pos :alive/position)
                      (:packages :alive/packages)
                      (:symbols :alive/symbols)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)))

(in-package :alive/lsp/completions)


(defparameter *kind-text* 1)
(defparameter *kind-method* 2)
(defparameter *kind-function* 3)
(defparameter *kind-constructor* 4)
(defparameter *kind-field* 5)
(defparameter *kind-variable* 6)
(defparameter *kind-class* 7)
(defparameter *kind-interface* 8)
(defparameter *kind-module* 9)
(defparameter *kind-property* 10)
(defparameter *kind-unit* 11)
(defparameter *kind-value* 12)
(defparameter *kind-enum* 13)
(defparameter *kind-keyword* 14)
(defparameter *kind-snippet* 15)
(defparameter *kind-color* 16)
(defparameter *kind-file* 17)
(defparameter *kind-reference* 18)
(defparameter *kind-folder* 19)
(defparameter *kind-enum-member* 20)
(defparameter *kind-constant* 21)
(defparameter *kind-struct* 22)
(defparameter *kind-event* 23)
(defparameter *kind-operator* 24)
(defparameter *kind-type-parameter* 25)


(defparameter *insert-plain* 1)
(defparameter *insert-snippet* 2)


(defun create-item (&key label kind doc-string insert-text insert-format)
    (let ((item (make-hash-table :test 'equalp)))

        (setf (gethash "label" item) label)
        (setf (gethash "kind" item) kind)
        (setf (gethash "documentation" item) doc-string)
        (setf (gethash "insertText" item) insert-text)
        (setf (gethash "insertTextFormat" item) insert-format)

        item))


(defun get-ext-symbols (pkg)
    (let ((inherited (list))
          (external (list)))

        (do-symbols (s pkg (if (< 0 (length external))
                               (mapcar #'string-downcase external)
                               (mapcar #'string-downcase inherited)))

            (multiple-value-bind (name status)

                    (find-symbol (string s) pkg)

                (cond ((eq status :external) (push name external))
                      ((eq status :inherited) (push name inherited)))))))


(defun to-item (name pkg-name)
    (let* ((lambda-list (symbols:get-lambda-list name pkg-name))
           (kind (if lambda-list
                     *kind-function*
                     *kind-text*))
           (text name)
           (insert-format (if lambda-list
                              *insert-snippet*
                              *insert-plain*))
           (doc-string (when lambda-list
                             (documentation (symbols:lookup name pkg-name) 'function))))

        (create-item :label name
                     :insert-text text
                     :kind kind
                     :doc-string doc-string
                     :insert-format insert-format)))


(defun symbols-to-items (&key name symbols pkg)
    (let ((pref (string-downcase name)))
        (mapcar (lambda (name)
                    (to-item name (package-name pkg)))
                (remove-if-not (lambda (str)
                                   (and (<= (length pref) (length str))
                                        (alive/utils:fuzzy-match pref str)))
                        symbols))))


(defun escape-name (name)
    (if (some (lambda (ch)
                  (and (alpha-char-p ch)
                       (lower-case-p ch))) name)
        (format nil "|~A|" name)
        (string-downcase name)))


(defun symbol-with-pkg (&key name num-colons pkg-name)
    (let* ((req-pkg (find-package (string-upcase pkg-name)))
           (pkg (if req-pkg req-pkg *package*)))

        (symbols-to-items :name name
                          :pkg pkg
                          :symbols (if (eq 1 num-colons)
                                       (get-ext-symbols pkg)
                                       (mapcar #'escape-name (symbols:get-all-names pkg))))))


(defun get-pkg-matches (&key name pkg-name)
    (let* ((pref (string-downcase name))
           (req-pkg (find-package (string-upcase pkg-name)))
           (current-pkg (if req-pkg req-pkg *package*)))

        (loop :with pkgs := ()
              :for pkg :in (list-all-packages)
              :do (push (package-name pkg) pkgs)
                  (setf pkgs (append pkgs (package-nicknames pkg)))
              :finally (return (symbols-to-items :name pref
                                                 :pkg current-pkg
                                                 :symbols (mapcar #'string-downcase pkgs))))))


(defun has-item (symbols item)
    (reduce (lambda (acc value)
                (or acc
                    (string= (gethash "label" value) (gethash "label" item))))
            symbols
        :initial-value NIL))


(defun create-set (pkgs symbols)
    (loop :with items := (copy-list pkgs)
          :for sym :in symbols
          :do (unless (has-item items sym)
                  (push sym items))
          :finally (return items)))


(defun symbol-no-pkg (&key name pkg-name)
    (let ((pkgs (get-pkg-matches
                    :name name
                    :pkg-name pkg-name))
          (symbols (symbol-with-pkg
                       :name name
                       :num-colons 0
                       :pkg-name pkg-name)))

        (cond ((and pkgs (not symbols)) pkgs)
              ((and (not pkgs) symbols) symbols)
              ((and pkgs symbols) (create-set pkgs symbols))
              (T '()))))


(defun feature (text)
    (when (or (string= (subseq text 0 2) "#+")
              (string= (subseq text 0 2) "#-"))
          (mapcar (lambda (name)
                      (create-item :label (format NIL "~A~A" (subseq text 0 2) (string-downcase name))
                                   :insert-text (format NIL "~A~A" (subseq text 0 2) (string-downcase name))
                                   :kind *kind-text*
                                   :doc-string nil
                                   :insert-format *insert-plain*))
                  *features*)))


(defun pound ()
    (mapcar (lambda (item)
                (create-item :label item
                             :insert-text item
                             :kind *kind-text*
                             :doc-string nil
                             :insert-format *insert-plain*))
            '("#+"
              "#-"
              "#/"
              "#\\"
              "#\\backspace"
              "#\\linefeed"
              "#\\newline"
              "#\\page"
              "#\\return"
              "#\\rubout"
              "#\\space"
              "#\\tab")))


(defun simple (&key text pos)
    (let* ((tokens (tokenizer:from-stream (make-string-input-stream text)))
           (pkg (packages:lookup (packages:for-pos text pos)))
           (*package* (if pkg pkg *package*)))

        (if (zerop (length tokens))
            '()
            (destructuring-bind (token1 token2 token3)
                    (symbols:find-tokens tokens pos)
                (cond ((and (eq (token:get-type-value token1) types:*symbol*)
                            (eq (token:get-type-value token2) types:*colons*)
                            (eq (token:get-type-value token3) types:*symbol*))
                          (symbol-with-pkg :name (token:get-text token1)
                                           :num-colons (length (token:get-text token2))
                                           :pkg-name (token:get-text token3)))

                      ((and (eq (token:get-type-value token1) types:*colons*)
                            (eq (token:get-type-value token2) types:*symbol*))
                          (symbol-with-pkg :name ""
                                           :num-colons (length (token:get-text token1))
                                           :pkg-name (token:get-text token2)))

                      ((and (eq (token:get-type-value token1) types:*symbol*)
                            (eq (token:get-type-value token2) types:*colons*))
                          (symbol-no-pkg :name (token:get-text token1)
                                         :pkg-name (package-name *package*)))

                      ((eq (token:get-type-value token1) types:*colons*)
                          (symbol-no-pkg :name ""
                                         :pkg-name (package-name *package*)))

                      ((and (eq (token:get-type-value token1) types:*symbol*)
                            (eq (token:get-type-value token2) types:*quote*))
                          (symbol-no-pkg :name (token:get-text token1)
                                         :pkg-name (package-name *package*)))

                      ((and (eq (token:get-type-value token1) types:*symbol*)
                            (eq (token:get-type-value token2) types:*back-quote*))
                          (symbol-no-pkg :name (token:get-text token1)
                                         :pkg-name (package-name *package*)))

                      ((eq (token:get-type-value token1) types:*symbol*)
                          (symbol-no-pkg :name (token:get-text token1)
                                         :pkg-name (package-name *package*)))

                      ((or (eq (token:get-type-value token1) types:*ifdef-false*)
                           (eq (token:get-type-value token1) types:*ifdef-true*))
                          (feature (token:get-text token1)))

                      ((eq (token:get-type-value token1) types:*macro*)
                          (pound))

                      (T '()))))))
