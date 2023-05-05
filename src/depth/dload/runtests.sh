BIN="../../../bin"

# pre-compile `use` function
${BIN}/uft es-vo use.scm > use.vo

# dload.scm
${BIN}/uft es-vo dload.scm > tmp.vo
${BIN}/svm use.vo tmp.vo

# good.scm
${BIN}/uft es-vo good.scm > tmp.vo
${BIN}/svm use.vo tmp.vo

# scheme105e.scm
${BIN}/uft es-vo scheme105e.scm > tmp.vo
${BIN}/svm use.vo tmp.vo

# clean up
rm tmp.vo use.vo