#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--syn-binding -g"

analyze --work=poc PoC/tb/common/my_config_ML505.vhdl
analyze --work=poc PoC/tb/common/my_project.vhdl
analyze --work=poc PoC/src/common/utils.vhdl
analyze --work=poc PoC/src/common/config.vhdl
analyze --work=poc PoC/src/common/strings.vhdl
analyze --work=poc PoC/src/common/vectors.vhdl
#analyze --work=poc PoC/src/common/board.vhdl
analyze --work=poc PoC/src/common/physical.vhdl
analyze --work=poc PoC/src/common/components.vhdl
analyze --work=poc PoC/tb/common/simulation.v93.vhdl
analyze --work=poc PoC/src/io/uart/uart.pkg.vhdl
analyze --work=poc PoC/src/io/uart/uart_bclk.vhdl
analyze --work=poc PoC/src/io/uart/uart_rx.vhdl
analyze --work=test PoC/tb/io/uart/uart_rx_tb.vhdl
elab_simulate --syn-binding --work=test uart_rx_tb --stop-time=15us

clean poc
clean test
echo "Test successful"
