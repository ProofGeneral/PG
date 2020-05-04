#!/bin/bash

# Print $1 in green
green () {
    echo -e "\e[32m$1\e[0m"
}

# Print $1 in red
red () {
    echo -e "\e[31m$1\e[0m"
}

assert () {
    if [ $# -lt 1 ]; then
        red "ERROR, assert expects some args (the code to run)"
        exit 1
    fi
    /bin/bash -exc '"$@"' bash "$@"
    ret=$?
    if [ "$ret" -ne 0 ]; then
        red "FAILURE, this shell command returned exit status $ret:
\$ $(printf "'%s' " "$@")\n"
        exit $ret
    fi
    echo
}

###############################################################################

assert emacs --version

rootdir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && cd .. && pwd )
srcdir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )

form="(progn (add-to-list 'load-path \"$rootdir\")
(add-to-list 'load-path \"$srcdir\")
(setq coq-test-dir \"$srcdir/\"))"  # we need a trailing slash here

assert emacs --batch -l ert --eval "$form" -l init-tests.el -l proof-general.el -l coq-tests.el -f ert-run-tests-batch-and-exit
