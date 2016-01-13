#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro 2>&1 | tee log.txt

if test `grep -c assert log.txt` -ne 2; then
    exit 1
else
    echo "Expected number of warnings"
fi

analyze repro1.vhdl
elab_simulate repro1

clean
rm log.txt

echo "Test successful"
