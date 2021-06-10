#! /bin/sh

. ../../testenv.sh

analyze_failure dff.vhdl 2> dff.out
diff dff.out dff.expected

rm -f dff.out
clean

echo "Test successful"
