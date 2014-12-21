#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate tb --fst=tb.fst
elab_simulate tb --vcd=tb.vcd

clean
rm -f tb.fst tb.vcd

echo "Test successful"
