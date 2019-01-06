#! /bin/sh

. ../../testenv.sh

analyze tb.vhdl 2> tb.out
diff --strip-trailing-cr -q tb.ref tb.out

rm -f tb.out
clean

echo "Test successful"
