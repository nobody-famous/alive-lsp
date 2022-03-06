(defpackage :alive/lsp/completions
    (:use :cl)
    (:export :simple)
    (:local-nicknames (:pos :alive/position)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)
                      (:types :alive/types)))

(in-package :alive/lsp/completions)


(defun find-tokens (tokens pos)
    (loop :for token :in tokens

          :collect token :into found-tokens

          :while (pos:less-than (token:end token) pos)

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


(defun symbol-with-pkg (&key name num-colons pkg-name)
    (let* ((pref (string-downcase name))
           (req-pkg (find-package (string-upcase pkg-name)))
           (pkg (if req-pkg req-pkg *package*)))

        (remove-if-not (lambda (str)
                           (and (< (length pref) (length str))
                                (string= pref (subseq str 0 (length pref)))))
                       (if (eq 1 num-colons)
                           (get-ext-symbols pkg)
                           (get-all-symbols pkg)))))


(defun simple (&key text pos)
    (let ((tokens (tokenizer:from-stream (make-string-input-stream text))))
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

                      ((eq (token:get-type-value token1) types:*symbol*)
                       (symbol-with-pkg :name (token:get-text token1)
                                        :num-colons 0
                                        :pkg-name (package-name *package*)))

                      (T (format T "~A ~A ~A~%" token3 token2 token1)))))))