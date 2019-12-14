#! /bin/sh

. ../../testenv.sh

analyze reader.vhdl
elab_simulate_failure reader

clean

echo "Test successful"
