#!/usr/bin/env bash

sbcl \
    --noinform \
    --load alive-lsp.asd \
    --eval "(asdf:load-system \"alive-lsp\")" \
    --eval "(defparameter *server* (alive/server:create))" \
    --eval "(alive/server:start *server*)"
