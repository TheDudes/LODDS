#!/bin/bash

sbcl --eval "(require \"sb-cover\")" \
     --eval "(declaim (optimize sb-cover:store-coverage-data))" \
     --eval "(asdf:oos 'asdf:load-op :cl-lodds :force t)" \
     --eval "(prove:run :cl-lodds)" \
     --eval "(sb-cover:report \"coverage/\")" \
     --eval "(declaim (optimize (sb-cover:store-coverage-data 0)))" \
     --eval "(cl-user::quit)"
