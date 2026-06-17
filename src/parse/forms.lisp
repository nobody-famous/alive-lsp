(defpackage :alive/parse/forms
    (:use :cl)
    (:export :xyz-from-stream
             :xyz-from-stream-or-nil
             :xyz-get-outer-form
             :xyz-get-nth-form
             :xyz-get-range-for-path
             :xyz-get-top-form)
    (:local-nicknames (:errors :alive/errors)
                      (:range :alive/range)
                      (:types :alive/types)
                      (:form :alive/parse/form)
                      (:pos :alive/position)
                      (:token :alive/parse/token)
                      (:tokenizer :alive/parse/tokenizer)))

(in-package :alive/parse/forms)


(declaim (optimize (speed 3)))


(declaim (type fixnum
               types:*open-paren*
               types:*symbol*
               types:*quote*
               types:*back-quote*
               types:*comma*
               types:*comma-at*))


(defstruct parse-state
    forms
    opens
    xyz-forms
    xyz-opens)


(defun xyz-open-paren (state token)
    (push (form:xyz-create :form-type types:*open-paren*
                           :tokens (list token))
          (parse-state-xyz-opens state)))


(defun xyz-is-open-paren (open-form)
    (and open-form
         (= types:*open-paren* (the fixnum (form:xyz-get-form-type open-form)))))


(defun xyz-is-symbol (open-form)
    (and open-form
         (= types:*symbol* (the fixnum (form:xyz-get-form-type open-form)))))


(defun xyz-is-quote (open-form)
    (and open-form
         (or (= types:*quote* (the fixnum (form:xyz-get-form-type open-form)))
             (= types:*back-quote* (the fixnum (form:xyz-get-form-type open-form))))))


(defun xyz-is-comma (open-form)
    (and open-form
         (or (= (the fixnum types:*comma*) (the fixnum (form:xyz-get-form-type open-form)))
             (= (the fixnum types:*comma-at*) (the fixnum (form:xyz-get-form-type open-form))))))


(defun xyz-collapse-opens (state &optional target)
    (loop :with prev := nil

          :for cur := (car (parse-state-xyz-opens state)) :do
              (when cur
                    (when prev
                          (form:xyz-add-kid cur prev))

                    (unless (eq (form:xyz-get-form-type cur) target)
                        (pop (parse-state-xyz-opens state))
                        (setf prev cur)))

          :while (and cur
                      (not (eq (form:xyz-get-form-type cur) target)))

          :finally (when (and prev
                              (not (parse-state-xyz-opens state)))
                         (push prev (parse-state-xyz-forms state)))))


(defun xyz-matched-close-paren (state token open-form)
    (form:xyz-add-token open-form token)

    (let ((next-open (car (parse-state-xyz-opens state))))
        (cond ((or (xyz-is-comma next-open)
                   (xyz-is-quote next-open))
                  (form:xyz-add-kid next-open open-form)
                  (xyz-collapse-opens state types:*open-paren*))

              ((xyz-is-open-paren next-open)
                  (form:xyz-add-kid (car (parse-state-xyz-opens state)) open-form))

              ((xyz-is-symbol next-open) nil)

              (T (push open-form (parse-state-xyz-forms state))))))


(defun unmatched-close-paren (state)
    (push (form:xyz-create :form-type types:*unmatched-close-paren*)
          (parse-state-xyz-forms state)))


(defun xyz-close-paren (state token)
    (xyz-collapse-opens state types:*open-paren*)

    (let ((open-form (pop (parse-state-xyz-opens state))))
        (if (xyz-is-open-paren open-form)
            (xyz-matched-close-paren state token open-form)
            (unmatched-close-paren state))))


(defun xyz-start-quote (state token)
    (let ((open-form (car (parse-state-xyz-opens state))))
        (cond ((xyz-is-quote open-form) NIL)
              (T (push (form:xyz-create :form-type (token:get-type-value token)
                                        :tokens (list token))
                       (parse-state-xyz-opens state))))))


(defun xyz-start-comma (state token)
    (let ((open-form (car (parse-state-xyz-opens state))))
        (cond ((xyz-is-comma open-form) NIL)
              (T (push (form:xyz-create :form-type (token:get-type-value token)
                                        :tokens (list token))
                       (parse-state-xyz-opens state))))))


(defun xyz-symbol-token (state token)
    (let ((open-form (car (parse-state-xyz-opens state))))

        (cond ((or (xyz-is-open-paren open-form)
                   (xyz-is-quote open-form))
                  (push (form:xyz-create :form-type types:*symbol*
                                         :tokens (list token))
                        (parse-state-xyz-opens state)))

              ((xyz-is-symbol open-form)
                  (form:xyz-add-token open-form token))

              (T (push (form:xyz-create :form-type types:*symbol*
                                        :tokens (list token))
                       (parse-state-xyz-opens state))))))


(defun xyz-white-space (state)
    (xyz-collapse-opens state types:*open-paren*))


(defun xyz-from-stream (input)
    (loop :with state := (make-parse-state)

          :for token :in (tokenizer:from-stream input) :do

              (cond ((token:is-type types:*open-paren* token) (xyz-open-paren state token))

                    ((token:is-type types:*close-paren* token) (xyz-close-paren state token))

                    ((or (token:is-type types:*quote* token)
                         (token:is-type types:*back-quote* token)) (xyz-start-quote state token))

                    ((or (token:is-type types:*comma* token)
                         (token:is-type types:*comma-at* token)) (xyz-start-comma state token))

                    ((token:is-type types:*ws* token) (xyz-white-space state))

                    ((or (token:is-type types:*line-comment* token)
                         (token:is-type types:*block-comment* token)
                         (token:is-type types:*ifdef-true* token))
                        NIL)

                    ((token:is-type types:*ifdef-false* token)
                        (if (parse-state-xyz-opens state)
                            (form:xyz-add-kid (car (parse-state-xyz-opens state))
                                              (form:xyz-create :form-type types:*ifdef-false*
                                                               :tokens (list token)))
                            (push (form:xyz-create :form-type types:*ifdef-false*
                                                   :tokens (list token))
                                  (parse-state-xyz-forms state))))

                    (T (xyz-symbol-token state token)))

          :finally (progn (xyz-collapse-opens state)
                          (return (reverse (parse-state-xyz-forms state))))))


(defun xyz-from-stream-or-nil (input)
    (handler-case
            (xyz-from-stream input)
        (T (c)
           (declare (ignore c))
           nil)))


(defun xyz-get-nth-form (forms n)
    (declare (type fixnum n))

    (loop :with counted := 0
          :with cur-form := nil

          :while (<= (the fixnum counted) n)

          :do (setf cur-form (pop forms))

              (cond ((or (eq types:*line-comment* (form:xyz-get-form-type cur-form))
                         (eq types:*block-comment* (form:xyz-get-form-type cur-form)))
                        NIL)

                    ((eq types:*ifdef-false* (form:xyz-get-form-type cur-form))
                        (pop forms))

                    (T (incf counted)))

          :finally (return cur-form)))


(defun xyz-get-top-form (forms pos)
    (loop :with top-form := nil

          :for form :in forms :do
              (when (and (pos:less-or-equal (form:xyz-get-start form) pos)
                         (pos:less-or-equal pos (form:xyz-get-end form)))
                    (setf top-form form))

          :finally (return top-form)))


(defun xyz-find-inner-form (form pos)
    (let ((start (form:xyz-get-start form))
          (end (form:xyz-get-end form))
          (kids (form:xyz-get-kids form)))

        (if (and kids
                 (pos:less-or-equal start pos)
                 (pos:less-than pos end))

            (loop :with target := form
                  :for kid :in kids
                  :do (let ((inner (xyz-find-inner-form kid pos)))
                          (when inner
                                (setf target inner)))
                  :finally (return target))

            nil)))


(defun xyz-get-outer-form (form pos)
    (when (and form
               (car (form:xyz-get-kids form)))
          (xyz-find-inner-form form pos)))


(defun xyz-get-range-for-path (forms source-path)
    (loop :with indicies := source-path
          :with ndx := nil
          :with form := nil

          :while indicies
          :do (setf ndx (pop indicies))

              (setf form (xyz-get-nth-form forms ndx))

              (unless form
                  (error (format nil "Source ndx ~A, path ~A, form ~A" ndx source-path form)))

              (setf forms (form:xyz-get-kids form))

          :finally (return (range:create (form:xyz-get-start form)
                                         (form:xyz-get-end form)))))
