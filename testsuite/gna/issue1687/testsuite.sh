#! /bin/sh

. ../../testenv.sh

analyze pkg.vhdl ent.vhdl
elab_simulate rc_add_n_f

clean

echo "Test successful"
