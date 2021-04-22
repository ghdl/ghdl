#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab repro
if ghdl_has_feature repro ghw; then
  simulate repro --wave=repro.ghw | tee repro.out
  ghw_diff repro
  rm -f repro.out repro.txt repro.ghw
fi

clean

echo "Test successful"
