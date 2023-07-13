#!/bin/bash

sbcl \
    --noinform \
    --non-interactive \
    --load alive-lsp.asd \
    --eval "(asdf:load-system \"alive-lsp/test\")" \
    --eval "(alive/test/coverage:run)"
