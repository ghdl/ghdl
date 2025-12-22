#! /bin/sh

. ../../testenv.sh

for f in badloc underscore1 quote1; do
  analyze_failure $f.vhdl 2> $f.err
  diff_nocr $f.err $f.ref
done

clean

echo "Test successful"
