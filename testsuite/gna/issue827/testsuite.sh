#! /bin/sh

. ../../testenv.sh

# A type conversion on the formal of an out port (LRM08 6.5.7.1) whose
# actual is a slice with generic-dependent bounds used to raise
# ASSERTION_ERROR at trans-chap3.adb:60 with the gcc and llvm backends:
# the anonymous subtype of the slice is only ever reached through that
# conversion, so its layout variable was never created.  mcode was not
# affected.  See issue827 (and the identical issue3088).
export GHDL_STD_FLAGS="--std=08"
analyze ent.vhd tb.vhd
elab_simulate tb

clean

# That layout must be a variable of the instance, not a global one: with
# two instances of the same architecture and a different generic, sharing
# it made the conversion use the bounds of the other instance.
analyze multi.vhd
elab_simulate multi

clean

# Same conversion on the port map of a block header, which has no
# instance record to put that layout in.
analyze blk.vhd
elab_simulate blk

clean

echo "Test successful"
