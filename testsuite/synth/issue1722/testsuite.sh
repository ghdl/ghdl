#! /bin/sh

. ../../testenv.sh

# A memory array written by a plain concurrent (unclocked) signal
# assignment -- rcells_memory(write_ptr) <= data_in; -- was accepted as a
# RAM write port and routed through the synchronous Id_Mem_Wr_Sync gate
# with no clock connected, which crashed --synth's netlist display with
# ASSERTION_ERROR at netlists.adb:886.
#
# An unclocked write port is no longer accepted as a RAM write port, so
# the array is synthesized as ordinary combinational dyn_insert/extract
# logic instead: no crash, and no "found RAM" note either, since the
# design does not describe a RAM.  See issue1722.
export GHDL_STD_FLAGS="--std=08"

synth fifo.vhd -e fifo > syn_fifo.vhdl 2>&1

if grep -q "Id_Mem_Wr_Sync\|ASSERTION_ERROR\|GHDL Bug occurred" syn_fifo.vhdl; then
  echo "FAIL: unexpected crash/error output"
  cat syn_fifo.vhdl
  exit 1
fi

if grep -q "found RAM" syn_fifo.vhdl; then
  echo "FAIL: the unclocked write is still inferred as a RAM"
  exit 1
fi

if ! grep -q "entity fifo is" syn_fifo.vhdl; then
  echo "FAIL: no netlist was produced"
  cat syn_fifo.vhdl
  exit 1
fi

clean

echo "Test successful"
