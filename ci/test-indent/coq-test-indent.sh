
# This script should be launched from its own directory, so that file
# coq-test-indent.el is accessible.

GREEN='\033[1;32m'
RED='\033[1;31m'
MAGENTA='\033[1;35m'
NC='\033[0m' # No Color

TESTFILE=$1
INDENTEDTESTFILE=indented_$1

BOXED="nil" 
if [[ "$1" == *"boxed.v" ]];
then
    BOXED="t"
fi

echo "cp $TESTFILE $INDENTEDTESTFILE"
cp $TESTFILE $INDENTEDTESTFILE

emacs -q -batch --eval "(progn (load-file \"coq-test-indent.el\") (launch-test \"$TESTFILE\" \"$INDENTEDTESTFILE\" $BOXED))" 

# echo "grep \"INDENT CHANGED\" $INDENTEDTESTFILE"
# grep "INDENT CHANGED" $INDENTEDTESTFILE
echo -n " diff -q $TESTFILE $INDENTEDTESTFILE..."
diff -q $TESTFILE $INDENTEDTESTFILE > /dev/null
if [[ $? == 1 ]] ;
then echo " DIFFERENCES FOUND" 
     printf "${RED} TEST FAILURE ***${NC}\n"
     echo "  *** details can be seen with:"
     echo "  ***  diff --side-by-side --suppress-common-lines $TESTFILE $INDENTEDTESTFILE"
     echo "  *** or graphically: "
     echo "  ***  meld $TESTFILE $INDENTEDTESTFILE"
else echo "NO DIFFERENCE"
     printf "${GREEN} TEST SUCCESS *** ${NC}\n"
fi
