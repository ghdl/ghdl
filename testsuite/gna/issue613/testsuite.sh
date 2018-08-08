#! /bin/sh

. ../../testenv.sh

if ! $GHDL --help -a | grep -q time-resolution; then
  echo "option --time-resolution not available"
else
    # Below the resolution
    analyze_failure --time-resolution=min ent.vhdl
    
    # Zero physical literals are always allowed.
    analyze --time-resolution=sec t1.vhdl
    analyze --time-resolution=sec t2.vhdl

    analyze ent.vhdl
    elab_simulate --time-resolution=ms ent

    elab_simulate --time-resolution=auto ent
    clean
fi

echo "Test successful"
