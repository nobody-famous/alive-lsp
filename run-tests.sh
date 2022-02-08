#!/bin/bash

sbcl --non-interactive \
    --load alive-lsp.asd \
    --eval "(asdf:load-system \"alive-lsp/test\")" \
    --eval "(ql:quickload :fiveam)" \
    --load "test/suite.lisp" \
    --eval "(fiveam:explain! (fiveam:run 'should-pass))"
