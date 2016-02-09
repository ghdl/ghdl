#! /bin/sh

. ../../testenv.sh

analyze polyamplib.vhdl master_testbench3.vhdl
if ghdl_is_interpretation; then
    stop=10us
else
    stop=1ms
fi

elab_simulate master_testbench3 --stop-time=$stop

clean

echo "Test successful"
