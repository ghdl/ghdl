#! /bin/sh

. ../../testenv.sh

analyze demo.vhd
analyze tb_demo.vhd
# elab_simulate tb_demo --stop-time=300sec --wave=tb_demo.ghw
elab_simulate tb_demo

clean

echo "Test successful"
