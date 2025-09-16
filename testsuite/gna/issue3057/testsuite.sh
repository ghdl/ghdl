#! /bin/sh

. ../../testenv.sh

analyze attr.vhdl
elab_simulate variable_attribute

clean

echo "Test successful"
