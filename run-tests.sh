#!/bin/bash

sbcl --non-interactive \
    --load alive-lsp.asd \
    --eval "(ql:quickload :fiveam)" \
    --eval "(asdf:load-system \"alive-lsp/test\")" \
    --eval "(fiveam:run! 'lookup)"
