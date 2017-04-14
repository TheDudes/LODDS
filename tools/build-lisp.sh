#!/bin/bash

sbcl --eval "(asdf:operate :build-op :cl-lodds-qt :force T)"
./gen-icons-lisp.sh
mkdir -p ../cl-code/cl-lodds-qt/bin/res/
cp -r ../cl-code/res/* ../cl-code/cl-lodds-qt/bin/res/
