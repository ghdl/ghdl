#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

analyze repro2.vhdl
elab_simulate repro2

analyze --work=nuand util.vhdl
analyze fx3_model_unmodified.vhdl
elab_simulate fx3_model

clean nuand
clean

echo "Test successful"
