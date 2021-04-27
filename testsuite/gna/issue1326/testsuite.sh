#! /bin/sh

. ../../testenv.sh

analyze mytestbench.vhdl
elab mytestbench
if ghdl_has_feature mytestbench ghw; then
  elab_simulate mytestbench --wave=dump.ghw | tee mytestbench.out

  # We're just checking that ghwdump doesn't crash on a zero length signal.
  ghw_dump dump

  rm -f mytestbench.out dump.txt dump.ghw
fi

clean

echo "Test successful"
