(in-package :asdf-user)


(defsystem "foo"
    :description "Test System"
    :components ((:module "bar"
                          :components ((:file "baz")))))


(defsystem "foo/2"
    :description "Test System"
    :components ((:module "bar"
                          :components ((:file "good")))))
