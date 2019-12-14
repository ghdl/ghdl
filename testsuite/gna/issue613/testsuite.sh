#! /bin/sh

. ../../testenv.sh

if ! $GHDL --help -a | grep -q time-resolution; then
  echo "option --time-resolution not available"
else
    # Below the resolution
    analyze_failure --time-resolution=sec ent.vhdl
    analyze_failure --time-resolution=ns t3.vhdl

    # Zero physical literals are always allowed.
    analyze --time-resolution=ms t1.vhdl
    analyze --time-resolution=ms t2.vhdl

    analyze ent.vhdl
    elab_simulate --time-resolution=us ent

    elab_simulate --time-resolution=auto ent

    analyze t3.vhdl
    elab_simulate --time-resolution=ps t3
    elab_simulate --time-resolution=auto t3

    analyze t87.vhdl
    elab_simulate --time-resolution=ps t87
    elab_simulate --time-resolution=auto t87
    clean

    GHDL_STD_FLAGS=--std=87
    analyze t87.vhdl
    elab_simulate --time-resolution=ps t87
    elab_simulate_failure --time-resolution=auto t87
    clean
fi

echo "Test successful"
