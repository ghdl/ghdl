#! /bin/sh

. ../../testenv.sh

for t in repro1 repro2 repro3 repro4 repro5 test_case; do
    synth_only $t
done

echo "Test successful"
