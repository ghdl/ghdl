#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl
elab_simulate_failure tb

analyze repro.vhdl
elab_simulate_failure repro --max-stack-alloc=64
elab_simulate repro --max-stack-alloc=256

clean

echo "Test successful"
