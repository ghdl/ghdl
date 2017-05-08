#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

analyze repro2.vhdl
elab_simulate repro2

analyze t3.vhdl
elab_simulate t3

analyze t3b.vhdl
elab_simulate t3b

analyze t4.vhdl
elab_simulate t4

analyze t6.vhdl
elab_simulate t6

clean

echo "Test successful"
