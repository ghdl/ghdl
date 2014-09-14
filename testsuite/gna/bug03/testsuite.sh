#! /bin/sh

. ../../testenv.sh

analyze wor_std.vhdl
elab_simulate foe

clean

echo "Test successful"
