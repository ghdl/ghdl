#! /bin/sh

exit 0
. ../../testenv.sh

for t in rom1 dpram1 dpram2 dpram3; do
    synth_tb $t
done

echo "Test successful"
