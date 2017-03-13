#!/bin/bash

# test script harness for Proof General for Coq w/ XML protocol
# Author: Paul Steckler

# change directory if script run outside its own directory
if [ ! -d "$PWD/../../coq/test" ]; then
    echo "Changing directory to" $(dirname $0)
    cd $(dirname $0)
fi

PGHOME="$PWD/../.."

PROOF_SITE="$PGHOME/generic/proof-site.el"
LOG_FILE="test.log"

ALL_TESTS="example-test example-tokens-test"

# look for tests on command-line
TESTS=$* 

# if user does not supply tests on command line, run all of them
if [ "$TESTS" = "" ] ; then
    TESTS=$ALL_TESTS
fi

LOG_FILE="test.log"
EMACS_FLAGS="-q -geometry 100x60"

function run_test {
    EXPR="(progn (load-file \"$PROOF_SITE\")"$CODE")"

    # run Emacs in background
    emacs $EMACS_FLAGS -eval "$EXPR" &
    PID=$!
    # don't show "Killed" message 
    disown
    
    echo
    echo "TEST:" $TEST_NAME
    echo
    echo "WHAT TO LOOK FOR:"
    echo
    echo "$ADVICE"
    echo
    
    read -p "Was the result OK? (y/N):" REPLY
    
    if [ "$REPLY" = "y" ] || [ "$REPLY" = "Y" ] ; then
	RESULT="OK"
    else
	RESULT="BAD"
    fi

    # silently kill Emacs
    kill -9 $PID >& /dev/null

    echo "TEST:" $TEST_NAME "RESULT:" $RESULT | tee --append $LOG_FILE
}

rm -f $LOG_FILE

# run the tests

for test_file in $TESTS; do
    source tests/$test_file
    TEST_NAME=$test_file
    run_test
done    

# print out log file for user

echo
read -p "View all test results? (Y/N):" REPLY

# use AWK to format the results nicely
    
if [ "$REPLY" = "y" ] || [ "$REPLY" = "Y" ] ; then
    echo
    echo "Showing results from log file:" $LOG_FILE
    echo
    echo "ALL RESULTS:"
    echo
    cat $LOG_FILE | awk '{ printf("%s %-25s %s %-5s\n",$1,$2,$3,$4) }'
fi

echo
echo Done.
