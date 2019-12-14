#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze repro.vhdl
if ghdl_has_feature repro dump-rti; then
    elab_simulate repro --dump-rti
fi

clean

analyze --work=poc my_config_ML505.vhdl
analyze --work=poc my_project.vhdl
analyze --work=poc utils.vhdl
analyze --work=poc config.vhdl
analyze --work=poc strings.vhdl
analyze --work=poc vectors.vhdl
analyze --work=poc physical.vhdl
analyze --work=poc sim_types.vhdl
analyze --work=poc sim_protected.v08.vhdl
analyze --work=poc sim_global.v08.vhdl
analyze --work=poc sim_simulation.v08.vhdl

analyze --work=poc arith.pkg.vhdl
analyze --work=poc arith_addw.vhdl
analyze arith_addw_tb.vhdl
elab_simulate arith_addw_tb --stop-time=100ns # --wave=arith.ghw 

clean
clean poc

echo "Test successful"
