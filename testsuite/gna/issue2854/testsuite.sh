#! /bin/sh

. ../../testenv.sh

analyze mwe.vhdl
elab_simulate mwe

analyze mwe1.vhdl
elab_simulate mwe1

analyze mwe2.vhdl
elab_simulate mwe2

clean

echo "Test successful"
