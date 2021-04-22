#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test_pkg.vhdl test_bench.vhdl
elab test_bench
if ghdl_has_feature test_bench ghw; then
  simulate test_bench --stop-time=700ns --wave=w.ghw
  ghw_diff w
  rm -f w.txt w.ghw
else
  simulate test_bench --stop-time=700ns
fi

clean
rm -f output.txt

echo "Test successful"
