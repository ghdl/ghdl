#! /bin/sh

. ../../testenv.sh

analyze attrs_pkg.vhdl ent1.vhdl ent2.vhdl ent3.vhdl uattr3.vhdl
elab_simulate uattr3

analyze ent4.vhdl
analyze_failure uattr4.vhdl

clean

echo "Test successful"
