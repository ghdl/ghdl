#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

FILES="
burst_ctrl.vhd
avm_master.vhd
avm_pause.vhd
avm_master_general.vhd
avm_arbit.vhd
avm_memory.vhd
tb_avm_arbit.vhd
"
analyze $FILES
elab_simulate_failure tb_avm_arbit --assert-level=error --stop-time=50us

clean

echo "Test successful"
