(defpackage :alive/test/compat/sbcl/symbols)


(fiveam:def-suite alive-lsp/sbcl/symbols
    :in alive-lsp)


(fiveam:in-suite alive-lsp/sbcl/symbols)


(fiveam:test lookup ()
    (fiveam:is (alive/symbols:callable-p "callable-p" "alive/symbols"))
    (fiveam:is (not (alive/symbols:callable-p "callable-p" "foo"))))
