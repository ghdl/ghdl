#! /bin/sh

. ../../testenv.sh

for f in e e2 e2b e2c e2d e3; do
    analyze $f.vhdl
    elab_simulate $f
done

clean

echo "Test successful"
