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


(defmethod clue:are-equal ((a alive/lsp/completions::item) b)
    (and (equal (type-of a) (type-of b))
         (string-equal (alive/lsp/completions::label a) (alive/lsp/completions::label b))
         (string-equal (alive/lsp/completions::insert-text a) (alive/lsp/completions::insert-text b))
         (eq (alive/lsp/completions::kind a) (alive/lsp/completions::kind b))))


(defmethod clue:are-equal ((a alive/lsp/message/initialize::client-info) b)
    (and (equal (type-of a) (type-of b))
         (string-equal (alive/lsp/message/initialize::name a) (alive/lsp/message/initialize::name b))
         (string-equal (alive/lsp/message/initialize::version a) (alive/lsp/message/initialize::version b))))


(defmethod clue:are-equal ((a alive/lsp/message/initialize::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/initialize::client-info a) (alive/lsp/message/initialize::client-info b))
         (string-equal (alive/lsp/message/initialize::locale a) (alive/lsp/message/initialize::locale b))
         (string-equal (alive/lsp/message/initialize::root-path a) (alive/lsp/message/initialize::root-path b))
         (string-equal (alive/lsp/message/initialize::root-uri a) (alive/lsp/message/initialize::root-uri b))))


(defmethod clue:are-equal ((a alive/lsp/message/initialize::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/initialize::sem-tokens-legend) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/initialize::token-types a) (alive/lsp/message/initialize::token-types b))))


(defmethod clue:are-equal ((a alive/lsp/message/initialize::sem-tokens-opts) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/initialize::legend a) (alive/lsp/message/initialize::legend b))
         (eq (alive/lsp/message/initialize::full a) (alive/lsp/message/initialize::full b))))


(defmethod clue:are-equal ((a alive/lsp/message/initialize::on-type-opts) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/initialize::first-trigger-character a) (alive/lsp/message/initialize::first-trigger-character b))
         (clue:are-equal (alive/lsp/message/initialize::more-trigger-characters a) (alive/lsp/message/initialize::more-trigger-characters b))))


(defmethod clue:are-equal ((a alive/lsp/message/initialize::completion-opts) b)
    (equal (type-of a) (type-of b)))


(defmethod clue:are-equal ((a alive/lsp/message/initialize::server-capabilities) b)
    (and (equal (type-of a) (type-of b))
         (eq (alive/lsp/message/initialize::text-document-sync a) (alive/lsp/message/initialize::text-document-sync b))
         (eq (alive/lsp/message/initialize::hover-provider a) (alive/lsp/message/initialize::hover-provider b))
         (eq (alive/lsp/message/initialize::document-range-formatting-provider a) (alive/lsp/message/initialize::document-range-formatting-provider b))
         (clue:are-equal (alive/lsp/message/initialize::document-on-type-formatting-provider a) (alive/lsp/message/initialize::document-on-type-formatting-provider b))
         (clue:are-equal (alive/lsp/message/initialize::semantic-tokens-provider a) (alive/lsp/message/initialize::semantic-tokens-provider b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/did-open::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/document/did-open::text-document a) (alive/lsp/message/document/did-open::text-document b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/did-open::did-open) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/types/text-doc::text-document) b)
    (and (equal (type-of a) (type-of b))
         (string-equal (alive/lsp/types/text-doc::uri a) (alive/lsp/types/text-doc::uri b))
         (equalp (alive/lsp/types/text-doc::version a) (alive/lsp/types/text-doc::version b))))


(defmethod clue:are-equal ((a alive/lsp/types/text-doc-item::text-document-item) b)
    (and (equal (type-of a) (type-of b))
         (string-equal (alive/lsp/types/text-doc-item::uri a) (alive/lsp/types/text-doc-item::uri b))
         (string-equal (alive/lsp/types/text-doc-item::language-id a) (alive/lsp/types/text-doc-item::language-id b))
         (string-equal (alive/lsp/types/text-doc-item::version a) (alive/lsp/types/text-doc-item::version b))
         (string-equal (alive/lsp/types/text-doc-item::text a) (alive/lsp/types/text-doc-item::text b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/did-change::did-change) b)
    (and (equal (type-of a) (type-of b))
         (string-equal (alive/lsp/message/abstract:method-name a) (alive/lsp/message/abstract:method-name b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/did-change::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/document/did-change::text-document a) (alive/lsp/message/document/did-change::text-document b))
         (clue:are-equal (alive/lsp/message/document/did-change::content-changes a) (alive/lsp/message/document/did-change::content-changes b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/did-change::content-change) b)
    (and (equal (type-of a) (type-of b))
         (string-equal (alive/lsp/message/document/did-change::text a) (alive/lsp/message/document/did-change::text b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/sem-tokens-full::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:method-name a) (alive/lsp/message/abstract:method-name b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/sem-tokens-full::req-params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/document/sem-tokens-full::text-document a) (alive/lsp/message/document/sem-tokens-full::text-document b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/load-file::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/load-file::params) b)
    (and (equal (type-of a) (type-of b))
         (string= (alive/lsp/message/alive/load-file::path a) (alive/lsp/message/alive/load-file::path b))
         (equalp (alive/lsp/message/alive/load-file::show-stdout a) (alive/lsp/message/alive/load-file::show-stdout b))
         (equalp (alive/lsp/message/alive/load-file::show-stderr a) (alive/lsp/message/alive/load-file::show-stderr b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/completion::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:method-name a) (alive/lsp/message/abstract:method-name b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/completion::req-params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/document/completion::text-document a) (alive/lsp/message/document/completion::text-document b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/top-form::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/top-form::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/top-form::text-document a) (alive/lsp/message/alive/top-form::text-document b))
         (clue:are-equal (alive/lsp/message/alive/top-form::pos a) (alive/lsp/message/alive/top-form::pos b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/range-format::req-params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/document/range-format::text-document a) (alive/lsp/message/document/range-format::text-document b))
         (clue:are-equal (alive/lsp/message/document/range-format::range a) (alive/lsp/message/document/range-format::range b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/range-format::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:method-name a) (alive/lsp/message/abstract:method-name b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/range::range) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/range::start a) (alive/range::start b))
         (clue:are-equal (alive/range::end a) (alive/range::end b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/list-threads::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/kill-thread::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/kill-thread::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/kill-thread::id a) (alive/lsp/message/alive/kill-thread::id b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/list-packages::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/unexport-symbol::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/unexport-symbol::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/unexport-symbol::sym-name a) (alive/lsp/message/alive/unexport-symbol::sym-name b))
         (clue:are-equal (alive/lsp/message/alive/unexport-symbol::pkg-name a) (alive/lsp/message/alive/unexport-symbol::pkg-name b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/do-eval::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/do-eval::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/do-eval::pkg-name a) (alive/lsp/message/alive/do-eval::pkg-name b))
         (clue:are-equal (alive/lsp/message/alive/do-eval::text a) (alive/lsp/message/alive/do-eval::text b))
         (clue:are-equal (alive/lsp/message/alive/do-eval::store-result a) (alive/lsp/message/alive/do-eval::store-result b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/do-inspect::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/do-inspect::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/do-inspect::pkg-name a) (alive/lsp/message/alive/do-inspect::pkg-name b))
         (clue:are-equal (alive/lsp/message/alive/do-inspect::text a) (alive/lsp/message/alive/do-inspect::text b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/do-inspect-sym::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/do-inspect-sym::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/do-inspect-sym::pkg-name a) (alive/lsp/message/alive/do-inspect-sym::pkg-name b))
         (clue:are-equal (alive/lsp/message/alive/do-inspect-sym::sym a) (alive/lsp/message/alive/do-inspect-sym::sym b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/do-inspect-close:request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/do-inspect-close::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/do-inspect-close::id a) (alive/lsp/message/alive/do-inspect-close::id b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/get-pkg::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/get-pkg::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/get-pkg::text-document a) (alive/lsp/message/alive/get-pkg::text-document b))
         (clue:are-equal (alive/lsp/message/alive/get-pkg::pos a) (alive/lsp/message/alive/get-pkg::pos b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/list-asdf::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/load-asdf::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/load-asdf::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/load-asdf::name a) (alive/lsp/message/alive/load-asdf::name b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/hover::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:method-name a) (alive/lsp/message/abstract:method-name b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/hover::req-params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/document/hover::text-document a) (alive/lsp/message/document/hover::text-document b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/symbol::request) b)
    (and (equal (type-of a) (type-of b))
         (equalp (alive/lsp/message/abstract:id a) (alive/lsp/message/abstract:id b))
         (clue:are-equal (alive/lsp/message/abstract:method-name a) (alive/lsp/message/abstract:method-name b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/alive/symbol::req-params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/alive/symbol::text-document a) (alive/lsp/message/alive/symbol::text-document b))))


(defmethod clue:are-equal ((a alive/lsp/types/sem-tokens::token) b)
    (and (equal (type-of a) (type-of b))
         (eq (alive/lsp/types/sem-tokens::token-type a) (alive/lsp/types/sem-tokens::token-type b))
         (eq (alive/lsp/types/sem-tokens::line a) (alive/lsp/types/sem-tokens::line b))
         (eq (alive/lsp/types/sem-tokens::start-col a) (alive/lsp/types/sem-tokens::start-col b))
         (eq (alive/lsp/types/sem-tokens::end-col a) (alive/lsp/types/sem-tokens::end-col b))))


(defmethod clue:are-equal ((a alive/text-edit::change) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/text-edit::range a) (alive/text-edit::range b))
         (clue:are-equal (alive/text-edit::text a) (alive/text-edit::text b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/fmt-on-type::request) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/abstract:params a) (alive/lsp/message/abstract:params b))))


(defmethod clue:are-equal ((a alive/lsp/message/document/fmt-on-type::params) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/message/document/fmt-on-type::text-document a) (alive/lsp/message/document/fmt-on-type::text-document b))
         (clue:are-equal (alive/lsp/message/document/fmt-on-type::pos a) (alive/lsp/message/document/fmt-on-type::pos b))
         (clue:are-equal (alive/lsp/message/document/fmt-on-type::ch a) (alive/lsp/message/document/fmt-on-type::ch b))
         (clue:are-equal (alive/lsp/message/document/fmt-on-type::options a) (alive/lsp/message/document/fmt-on-type::options b))))


(defmethod clue:are-equal ((a alive/lsp/types/format-options::format-options) b)
    (and (equal (type-of a) (type-of b))
         (clue:are-equal (alive/lsp/types/format-options::indent-width a) (alive/lsp/types/format-options::indent-width b))))
