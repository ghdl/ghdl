#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 --latches"

synth_only repro3
synth_only repro4

synth_failure knn.vhdl argmin.vhdl distancias.vhdl insere.vhdl moda.vhdl norm.vhdl pacote_aux.vhdl top.vhdl -e > syn_top.vhdl


echo "Test successful"

