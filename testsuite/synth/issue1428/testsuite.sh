#! /bin/sh

. ../../testenv.sh

for t in rec1 rec2 rec3 repro1 repro2 repro3 repro4b repro4 repro5b repro5 repro6
do
    synth --expect-failure $t.vhdl -e 2>&1 | grep $t.vhdl: > $t.out
    diff_nocr $t.out $t.ref
done

echo "Test successful"
