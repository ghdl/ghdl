#! /bin/sh

. ../../testenv.sh

analyze *.vhd
if ghdl_is_interpretation; then
    elab_simulate tb --stop-time=200ns
else
    elab_simulate tb --stop-time=4us
fi

clean

echo "Test successful"
