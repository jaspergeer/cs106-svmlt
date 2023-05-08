#!/bin/bash

BIN="../../../bin"

# pre-compile `use` function
${BIN}/uft es-vo use.scm > use.vo

TESTS=("simple" "recurse" "overcurry" "args")

for t in ${TESTS[@]}
do
  echo "running ${t}..."
  ${BIN}/uft es-vo ${t}.scm > tmp.vo
  ${BIN}/svm use.vo tmp.vo
  echo ""
done

rm use.vo tmp.vo
