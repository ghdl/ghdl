#! /bin/sh

. ../../testenv.sh


analyze resolv1.vhdl
elab_simulate top

analyze resolv2.vhdl
elab_simulate top

clean

echo "Test successful"
