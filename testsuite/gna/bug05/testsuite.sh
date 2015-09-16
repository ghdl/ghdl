#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab tb
if ghdl_has_feature tb fst; then
  simulate tb --fst=tb.fst
fi
simulate tb --vcd=tb.vcd

clean
rm -f tb.fst tb.vcd

echo "Test successful"
