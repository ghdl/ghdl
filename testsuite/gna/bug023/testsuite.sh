#! /bin/sh

. ../../testenv.sh

analyze ppkg.vhdl ppkg_tb.vhdl
elab_simulate ppkg_tb

analyze ppkg1.vhdl ppkg1_tb.vhdl
elab_simulate ppkg1_tb

clean

echo "Test successful"
