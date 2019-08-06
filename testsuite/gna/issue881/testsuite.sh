#! /bin/sh

. ../../testenv.sh

# See #613

if ! $GHDL --help -a | grep -q time-resolution; then
  echo "option --time-resolution not available"
else
    analyze t87.vhdl
    elab_simulate --time-resolution=ps t87
    elab_simulate --time-resolution=auto t87

    analyze t87io.vhdl
    elab_simulate --time-resolution=ps t87io
    elab_simulate --time-resolution=auto t87io

    clean
fi

echo "Test successful"
