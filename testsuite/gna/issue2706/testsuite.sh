#! /bin/sh

. ../../testenv.sh


# Normal end
analyze tb1.vhdl
elab tb1
if ghdl_has_feature tb1 vcd; then
  elab_simulate tb1 --vcd=tb1.vcd
  # Last signal change is at 15ns (s toggles 4 times: 0->1->0->1->0)
  grep "#15000000" tb1.vcd

  # assertion failure
  analyze tb3.vhdl
  elab_simulate_failure tb3 --vcd=tb3.vcd
  # Assertion fails at 30ns.  Final timestamp should be present.
  # (s changes in a delta cycle at 30ns, but that's not captured as
  # VCD only records non-delta cycles)
  grep "#30000000" tb3.vcd

  clean
fi

echo "Test successful"
