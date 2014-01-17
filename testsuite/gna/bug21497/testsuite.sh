#! /bin/sh

. ../../testenv.sh


analyze repro.vhdl
elab_simulate tb

clean

echo "Test successful"
