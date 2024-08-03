#! /bin/sh

. ../../testenv.sh

# Normal end
analyze tb1.vhdl
elab_simulate tb1 --vcd=tb1.vcd
grep "#20000000" tb1.vcd

# assertion failure
analyze tb3.vhdl
elab_simulate_failure tb3 --vcd=tb3.vcd
grep "#25000000" tb3.vcd

clean

echo "Test successful"
