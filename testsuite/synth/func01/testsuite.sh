#! /bin/sh

. ../../testenv.sh

for t in func01 func02 func03 func04 func05 func06 func07 func08b func08; do
    synth_tb $t
done

echo "Test successful"
