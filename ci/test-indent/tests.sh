



for i in $(find . -name "*.v" -exec basename '{}' \; | grep -v "indented_") ; do
    echo -n "Testing $i..."
    ./coq-test-indent.sh $i 2>&1 | tee .$i.log | grep "\*\*\*"
done
