(defpackage :alive-lsp/suite)


(fiveam:def-suite alive-lsp
    :description "Test the Alive LSP server")


(fiveam:in-suite alive-lsp)


; (fiveam:test should-pass
;     (fiveam:is (alive/symbols:callable-p "callable-p" "alive/symbols"))
;     (fiveam:is (not (eq t nil))))


; (fiveam:test should-fail
;     (fiveam:is (eq t nil)))
