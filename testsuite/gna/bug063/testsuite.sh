#! /bin/sh

. ../../testenv.sh

analyze_failure dff.vhdl 2> dff.out
diff --strip-trailing-cr dff.out dff.expected

rm -f dff.out
clean

echo "Test successful"
