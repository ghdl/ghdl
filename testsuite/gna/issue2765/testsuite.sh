#! /bin/sh

. ../../testenv.sh

# The reported design constrains an unconstrained formal with a single
# 'downto' slice in the port map:
#
#   data_i(5 downto 0) => count_i
#
# LRM08 5.3.2.2 e) 2) gives the index range of such an object the direction
# of the corresponding index subtype of the base type of the interface
# object -- 'natural' for a std_ulogic_vector, hence 'to' -- whenever the
# object is associated by more than one association element or by a single
# one whose formal designator is a slice name.  The object is therefore
# ascending and this 'downto' slice of it is an error (LRM08 8.5).
#
# GHDL used to take the direction of the slice instead, which accepted the
# design, and this test used to run it.  That was the non-conformant half of
# a93226f23; the other half, not crashing after the error, is what #2765 was
# really about and is still checked below.  See also issue2688, where the
# same rule applies to a formal sliced in several parts.
analyze top.vhdl

out=$(elab_simulate top 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed after reporting the slice direction error"
  exit 1
fi

if ! echo "$out" | grep -q "slice direction doesn't match"; then
  echo "FAIL: expected the slice direction error"
  exit 1
fi

out=$(synth top 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: --synth crashed after reporting the slice direction error"
  exit 1
fi

if ! echo "$out" | grep -q "slice direction doesn't match"; then
  echo "FAIL: expected the slice direction error from --synth"
  exit 1
fi

# The same design written ascending is legal and must keep working.
analyze ok.vhdl
elab_simulate top_ok

synth top_ok > syn_top_ok.vhdl
rm -f syn_top_ok.vhdl

clean

echo "Test successful"
