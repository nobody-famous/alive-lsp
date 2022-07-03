(defpackage :alive/test/compare
    (:use :cl))

(in-package :alive/test/compare)


(defmethod clue:are-equal ((a alive/position::pos) b)
    (and (equal (type-of a) (type-of b))
         (eq (alive/position::line a) (alive/position::line b))
         (eq (alive/position::col a) (alive/position::col b))))


(defmethod clue:are-equal ((a alive/parse/token::token) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/parse/token::start a) (alive/parse/token::start b))
         (clue:are-equal (alive/parse/token::end a) (alive/parse/token::end b))
         (string-equal (alive/parse/token::text a) (alive/parse/token::text b))
         (eq (alive/parse/token::type-value a) (alive/parse/token::type-value b))))


(defmethod clue:are-equal ((a alive/parse/form::form) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/parse/form::form-type a) (alive/parse/form::form-type b))
         (clue:are-equal (alive/parse/form::start a) (alive/parse/form::start b))
         (clue:are-equal (alive/parse/form::end a) (alive/parse/form::end b))
         (clue:are-equal (alive/parse/form::in-pkg-p a) (alive/parse/form::in-pkg-p b))
         (clue:are-equal (alive/parse/form::kids a) (alive/parse/form::kids b))))
