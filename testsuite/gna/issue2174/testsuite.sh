#! /bin/sh

. ../../testenv.sh

analyze_failure test.vhdl

for e in t1 t2 t3; do
    analyze $e.vhdl
    elab_simulate $e
done

clean

echo "Test successful"
