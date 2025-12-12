#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab_simulate_failure tb

analyze repro.vhdl
if ! ghdl_is_interpretation; then
    elab_simulate_failure repro --max-stack-alloc=64
fi
elab_simulate repro --max-stack-alloc=256

clean

echo "Test successful"
