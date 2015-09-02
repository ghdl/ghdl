#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro_ent

#analyze repro1.vhdl
#elab_simulate repro1_ent

clean

echo "Test successful"
