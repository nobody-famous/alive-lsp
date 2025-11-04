#!/usr/bin/env bash

sbcl \
    --noinform \
    --eval "(require 'asdf)" \
    --eval "(asdf:load-asd (truename \"alive-lsp.asd\"))" \
    --eval "(defparameter *server* (alive/server:create))" \
    --eval "(alive/server:start *server*)"
