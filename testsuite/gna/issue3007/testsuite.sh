#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mwe.vhdl
elab_simulate tb_mwe

analyze mwe_sig.vhdl
elab_simulate mwe_sig

analyze mwe_var.vhdl
elab_simulate mwe_var

clean

echo "Test successful"
