#!/bin/sh

. ../../testenv.sh

analyze hello.vhdl
elab_simulate hello

clean

echo "test successful"
