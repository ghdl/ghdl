#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze ent.vhdl
elab_simulate test

for t in repro1 repro2 repro3 repro4; do
    analyze $t.vhdl
    elab_simulate $t
done

clean

# From synthworks

analyze test_2b_record_subtype_alias/TestPkg.vhd
analyze test_2b_record_subtype_alias/Test.vhd
analyze test_2b_record_subtype_alias/TbTest.vhd
simulate TbTest

clean

echo "Test successful"
