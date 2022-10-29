#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
for f in free_slice_name.vhdl to_string_real-1.vhdl to_string_real-2.vhdl to_string_real-3.vhdl; do
  run $GHDL -s $GHDL_STD_FLAGS --expect-failure $f
done

clean

echo "Test successful"
