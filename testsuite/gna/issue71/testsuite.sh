#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
if ghdl_has_feature repro1 fst; then
    elab_simulate repro1 --fst=w1.fst
fi

rm -f w1.fst
clean

echo "Test successful"
