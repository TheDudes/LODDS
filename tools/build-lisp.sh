#!/bin/bash
set -e

LODDS=../
FOLDER=./lodds/
TARGET=./lodds.tar.gz

sbcl --eval "(asdf:operate :build-op :cl-lodds-qt :force T)"

${LODDS}tools/gen-icons-lisp.sh

mkdir -p ${FOLDER}res/
cp -r ${LODDS}cl-code/cl-lodds-qt/bin/* ${FOLDER}
cp -r ${LODDS}cl-code/res/* ${FOLDER}res/

tar -zcvf ${TARGET} ${FOLDER}
