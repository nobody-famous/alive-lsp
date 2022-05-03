(defpackage :alive/lsp/completions
    (:use :cl)
    (:export :create-item
             :simple

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


(defclass item ()
        ((label :accessor label
                :initform nil
                :initarg :label)
         (insert-text :accessor insert-text
                      :initform nil
                      :initarg :insert-text)
         (insert-text-format :accessor insert-text-format
                             :initform 1
                             :initarg :insert-text-format)
         (kind :accessor kind
               :initform nil
               :initarg :kind)
         (documentation :accessor doc-string
                        :initform nil
                        :initarg :doc-string)))


(defmethod types:deep-equal-p ((a item) b)
    (and (equal (type-of a) (type-of b))
        (string-equal (label a) (label b))
        (string-equal (insert-text a) (insert-text b))
        (eq (kind a) (kind b))))


(defmethod print-object ((obj item) out)
    (format out "{label: ~A; insertText: ~A; kind: ~A; documentation: ~A}"
        (label obj)
        (insert-text obj)
        (kind obj)
        (doc-string obj)))


(defun create-item (&key label kind doc-string insert-text insert-format)
    (make-instance 'item
        :label label
        :kind kind
        :doc-string doc-string
        :insert-text insert-text
        :insert-text-format insert-format))


(defun find-tokens (tokens pos)
    (loop :for token :in tokens

        :collect token :into found-tokens

        :while (pos:less-than (token:get-end token) pos)

        :finally (return (cond ((<= 3 (length found-tokens)) (subseq (reverse found-tokens) 0 3))
                             ((= 2 (length found-tokens)) (reverse (cons nil found-tokens)))
                             ((= 1 (length found-tokens)) (list (first found-tokens) nil nil))))))


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


(defun get-all-symbols (pkg)
    (let ((syms (list)))
        (do-symbols (s pkg syms)
            (push (string-downcase (string s)) syms))))


(defun list-to-snippet (name lambda-list)
    (loop :with is-keys := nil
        :with skip-rest := nil
        :with ndx := 1
        :with items := '()

        :for item :in lambda-list :do
        (cond ((not (eq 'symbol (type-of item)))
               (string-downcase (format NIL "${~A:~A}~%" ndx item))
               (incf ndx))
            ((string= "&KEY" item) (setf is-keys T))
            ((char= #\& (char (string item) 0)) (setf skip-rest T))
            ((not skip-rest) (setf items
                                 (cons
                                     (let ((item-text (string-downcase item)))
                                         (if is-keys
                                             (format nil ":~A ${~A:~A}" item-text ndx item-text)
                                             (format nil "${~A:~A}" ndx (string-downcase item))))

                                     items))
                             (incf ndx)))

        :finally (return (format nil "~{~A~^ ~}" (cons name (reverse items))))))


(defun to-snippet (name lambda-list)
    (when (and (eq (type-of lambda-list) 'cons)
              (or (not (cdr lambda-list))
                  (eq (type-of (cdr lambda-list)) 'cons)))
        (list-to-snippet name lambda-list)))


(defun to-item (name pkg-name)
    (let* ((lambda-list (symbols:get-lambda-list name pkg-name))
           (kind (if lambda-list
                     *kind-function*
                     *kind-text*))
           (text (if lambda-list
                     (to-snippet name lambda-list)
                     name))
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


(defun strict-match (pref str)
    (string= pref (subseq str 0 (length pref))))


(defun get-found-chars (str)
    (loop :with found := (make-hash-table)
        :for ch :across str :do
        (setf (gethash ch found) T)
        :finally (return found)))


(defun fuzzy-match (pref str)
    (cond ((zerop (length pref)) T)
        ((zerop (length str)) NIL)
        (T (let ((found (get-found-chars str)))
               (loop :with match := (char= (char pref 0) (char str 0))
                   :for ch :across pref :do
                   (setf match
                       (and match
                           (gethash ch found)))
                   :finally (return match))))))


(defun symbols-to-items (&key name symbols pkg)
    (let ((pref (string-downcase name)))
        (mapcar (lambda (name)
                    (to-item name (package-name pkg)))
                (remove-if-not (lambda (str)
                                   (and (<= (length pref) (length str))
                                       (fuzzy-match pref str)))
                        symbols))))


(defun symbol-with-pkg (&key name num-colons pkg-name)
    (let* ((req-pkg (find-package (string-upcase pkg-name)))
           (pkg (if req-pkg req-pkg *package*)))

        (symbols-to-items :name name
                          :pkg pkg
                          :symbols (if (eq 1 num-colons)
                                       (get-ext-symbols pkg)
                                       (get-all-symbols pkg)))))


(defun get-pkg-matches (&key name pkg-name)
    (let* ((pref (string-downcase name))
           (req-pkg (find-package (string-upcase pkg-name)))
           (pkg (if req-pkg req-pkg *package*)))

        (symbols-to-items :name pref
                          :pkg pkg
                          :symbols (mapcar #'string-downcase
                                           (mapcar #'package-name
                                                   (list-all-packages))))))


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
            ((and pkgs symbols) (concatenate 'cons pkgs symbols))
            (T '()))))


(defun prefix-symbols (pref items)
    (loop :for item :in items :do
        (setf (label item) (format nil "~A~A" pref (label item)))
        (setf (insert-text item) (format nil "~A~A" pref (insert-text item)))

        :finally (return items)))


(defun simple (&key text pos)
    (let* ((tokens (tokenizer:from-stream (make-string-input-stream text)))
           (pkg (packages:lookup (packages:for-pos text pos)))
           (*package* (if pkg pkg *package*)))

        (if (zerop (length tokens))
            '()
            (destructuring-bind (token1 token2 token3) (find-tokens tokens pos)
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

                    ((eq (token:get-type-value token1) types:*colons*)
                     (symbol-with-pkg :name ""
                                      :num-colons (length (token:get-text token1))
                                      :pkg-name (package-name *package*)))

                    ((and (eq (token:get-type-value token1) types:*symbol*)
                         (eq (token:get-type-value token2) types:*quote*))
                     (prefix-symbols "'" (symbol-no-pkg :name (token:get-text token1)
                                                        :pkg-name (package-name *package*))))

                    ((and (eq (token:get-type-value token1) types:*symbol*)
                         (eq (token:get-type-value token2) types:*back-quote*))
                     (prefix-symbols "`" (symbol-no-pkg :name (token:get-text token1)
                                                        :pkg-name (package-name *package*))))

                    ((eq (token:get-type-value token1) types:*symbol*)
                     (symbol-no-pkg :name (token:get-text token1)
                                    :pkg-name (package-name *package*)))

                    (T '()))))))