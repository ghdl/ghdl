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

analyze test_1a_unsigned_port/TestPkg.vhd
analyze test_1a_unsigned_port/Test.vhd
analyze test_1a_unsigned_port/TbTest.vhd
elab_simulate TbTest

clean

analyze test_1b_unsigned_alias/TestPkg.vhd
analyze test_1b_unsigned_alias/Test.vhd
analyze test_1b_unsigned_alias/TbTest.vhd
elab_simulate TbTest

clean

analyze test_2a_record_subtype/TestPkg.vhd
analyze test_2a_record_subtype/Test.vhd
analyze test_2a_record_subtype/TbTest.vhd
elab_simulate TbTest

clean

analyze test_2b_record_subtype_alias/TestPkg.vhd
analyze test_2b_record_subtype_alias/Test.vhd
analyze test_2b_record_subtype_alias/TbTest.vhd
elab_simulate TbTest

clean

analyze test_2c_record_range_alias/TestPkg.vhd
analyze test_2c_record_range_alias/Test.vhd
analyze test_2c_record_range_alias/TbTest.vhd
elab_simulate TbTest

clean

analyze test_2d_record_subtype_length_decl/TestPkg.vhd
analyze test_2d_record_subtype_length_decl/Test.vhd
analyze test_2d_record_subtype_length_decl/TbTest.vhd
elab_simulate TbTest

clean

analyze test_2e_record_simple_alias/TestPkg.vhd
analyze test_2e_record_simple_alias/Test.vhd
analyze test_2e_record_simple_alias/TbTest.vhd
elab_simulate TbTest

clean

analyze test_2f_record_constrained_alias/TestPkg.vhd
analyze test_2f_record_constrained_alias/Test.vhd
analyze test_2f_record_constrained_alias/TbTest.vhd
elab_simulate TbTest

clean

analyze test_2g_record_alias_port/TestPkg.vhd
analyze test_2g_record_alias_port/Test.vhd
analyze test_2g_record_alias_port/TbTest.vhd
elab_simulate TbTest

clean

echo "Test successful"
