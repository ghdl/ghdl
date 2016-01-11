#! /bin/sh

. ../../testenv.sh

# analyze --work=poc PoC/tb/common/my_config_ML505.vhdl
# analyze --work=poc PoC/tb/common/my_project.vhdl
# analyze --work=poc PoC/src/common/utils.vhdl
# analyze --work=poc PoC/src/common/config.vhdl
# analyze --work=poc PoC/src/common/strings.vhdl
# analyze --work=poc PoC/src/common/vectors.vhdl
# #analyze --work=poc PoC/src/common/board.vhdl
# analyze --work=poc PoC/src/common/physical.vhdl
# analyze --work=poc PoC/src/common/components.vhdl
# analyze --work=poc PoC/tb/common/simulation.v93.vhdl

analyze --work=poc my_config_ML505.vhdl
analyze --work=poc my_project.vhdl
analyze --work=poc utils.vhdl
analyze --work=poc config.vhdl
analyze --work=poc strings.vhdl
analyze --work=poc vectors.vhdl
analyze --work=poc physical.vhdl
analyze --work=poc simulation.v93.vhdl

analyze --work=poc arith_prng.vhdl
analyze arith_prng_tb.vhdl
elab_simulate arith_prng_tb

clean
clean poc

echo "Test successful"
