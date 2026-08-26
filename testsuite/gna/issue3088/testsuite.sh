#! /bin/sh

. ../../testenv.sh

# Same defect as issue827, reported from the other side: a type
# conversion on the formal of an out port (unsigned(sum) => ...) whose
# actual slice is sized from a generic raised ASSERTION_ERROR at
# trans-chap3.adb:60 with the gcc and llvm backends.  mcode was not
# affected.  See issue3088.
export GHDL_STD_FLAGS="--std=08"
analyze ripple_carry_adder.vhd ripple_carry_adder_tb.vhd
elab_simulate ripple_carry_adder_tb

clean

echo "Test successful"
