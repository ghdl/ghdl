#! /bin/sh

. ../../testenv.sh

analyze pkg.vhdl
analyze types_pkg.vhdl
analyze tb.vhdl
elab design_tb

if ghdl_has_feature design_tb ghw; then
  simulate design_tb --wave=tb.ghw
  ghw_diff tb
  rm -f tb.txt tb.ghw
fi

clean

echo "Test successful"
