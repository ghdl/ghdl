#! /bin/sh

. ../../testenv.sh

analyze mcve.vhdl
elab mcve
if ghdl_has_feature mcve vcd; then
  simulate mcve --vcd=mcve.vcd --stop-time=1us

  if ! grep -q "stage_reg is not handled" mcve.vcd; then
    echo "error: stage_reg is not dumpable"
    exit 1;
  fi
fi

clean
rm -f mcve.vcd

echo "Test successful"
