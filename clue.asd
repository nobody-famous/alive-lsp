(in-package :asdf)

(defsystem "clue"
    :version "0.1.0"
    :author ""
    :license ""
    :depends-on ()
    :components ((:module "clue"
                          :components
                          ((:file "package")
                           (:file "formatting")
                           (:file "errors")
                           (:file "check")
                           (:file "run"))))
    :description ""
    :in-order-to ((test-op (test-op "clue/tests"))))
