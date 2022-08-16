#! /bin/sh

. ../../testenv.sh

for t in ram01 ram02; do
    synth_tb $t 2> $t.log
done

clean

echo "Test successful"
