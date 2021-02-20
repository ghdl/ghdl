#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
analyze absenc_pkg.vhdl absenc_utils.vhdl absenc_master.vhdl absenc_master_endat.vhdl endat_tb.vhdl
elab_failure endat_tb

analyze repro.vhdl
elab_failure repro

analyze_failure repro1.vhdl

clean

echo "Test successful"
