#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

clean

export GHDL_STD_FLAGS=--std=02
analyze file_reader_pkg.vhdl ex.vhdl tb_ex.vhdl
elab_simulate tbcrashexample2 -gvectorfilename=input.txt

clean

echo "Test successful"
