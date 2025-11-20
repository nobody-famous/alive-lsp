#!/usr/bin/env bash

sbcl \
    --noinform \
    --non-interactive \
    --eval "(require 'asdf)" \
    --eval "(asdf:load-asd (truename \"alive-lsp.asd\"))" \
    --eval "(asdf:load-system \"alive-lsp/test\")" \
    --eval "(alive/test/coverage:run)"
