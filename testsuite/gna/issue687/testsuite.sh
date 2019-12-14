#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

check_output()
{
    if ! grep -q "bound check failure" $1; then
	echo "missing bound check failure"
	exit 1
    fi
}

analyze ent.vhdl
elab_simulate_failure tb > tb.err
check_output tb.err

clean

analyze ent_ok.vhdl
elab_simulate tb

clean

analyze ent2.vhdl
elab_simulate_failure tb > tb.err
check_output tb.err

clean

analyze ent3.vhdl
elab_simulate_failure tb > tb.err
check_output tb.err

clean

analyze ent4.vhdl
elab_simulate_failure tb > tb.err
check_output tb.err

clean

rm -f tb.err

echo "Test successful"
