#! /bin/sh

. ../../testenv.sh

analyze demo.vhd
analyze tb_demo.vhd
if ghdl_is_interpretation; then
  elab_simulate tb_demo --stop-time=1sec
else
  # elab_simulate tb_demo --stop-time=300sec --wave=tb_demo.ghw
  elab_simulate tb_demo
fi

clean

echo "Test successful"
