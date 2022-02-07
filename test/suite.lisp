(defpackage :alive-lsp/suite)


(fiveam:def-suite alive-lsp
    :description "Test the Alive LSP server")


(fiveam:test checking
    (fiveam:is (eq t nil)))
