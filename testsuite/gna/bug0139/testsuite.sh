#! /bin/sh

. ../../testenv.sh

analyze_failure repro1.vhdl
analyze_failure repro2.vhdl 2> repro2.err
diff_nocr repro2.err repro2.ref
analyze_failure repro3.vhdl 2> repro3.err
diff_nocr repro3.err repro3.ref
analyze_failure repro4.vhdl 2> repro4.err
diff_nocr repro4.err repro4.ref

clean

echo "Test successful"
