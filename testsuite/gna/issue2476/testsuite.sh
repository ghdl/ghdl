#! /bin/sh

. ../../testenv.sh

for f in bug3 bug2 bug; do
    analyze $f.vhdl
    elab_simulate bug_from_2417_fix

    clean
done

echo "Test successful"
