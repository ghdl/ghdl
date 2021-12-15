#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth generic_sfifo.vhdl my_fifo.vhdl -e > syn_my_fifo.vhdl

analyze syn_my_fifo.vhdl tb_my_fifo.vhdl
elab_simulate tb_my_fifo
clean

echo "Test successful"
