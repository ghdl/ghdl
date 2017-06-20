#! /bin/sh

. ../../testenv.sh

analyze test_string.vhdl
elab test_string

if ghdl_has_feature test_string ghw; then
  simulate test_string --wave=sim.ghw --stop-time=20ns
fi

clean
if [ $# -eq 0 ]; then
  rm -f sim.ghw
fi


echo "Test successful"
