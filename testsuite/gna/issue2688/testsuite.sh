#! /bin/sh

. ../../testenv.sh

# Five shapes of the same port map, all taken from the issue2688 thread.
export GHDL_STD_FLAGS="--std=08"

analyze skid.vhdl

# 1. The original report: the actual for the unconstrained formal r_data is
#    a concatenation of two *signals*, so it is not a globally static
#    expression.  Per LRM08 6.5.6.3 that makes the port equivalent to an
#    anonymous signal of the formal's subtype, which is unconstrained --
#    illegal.  This used to end in an internal error naming
#    IIR_KIND_CONCATENATION_OPERATOR; it is now a clean diagnostic.
out=$(analyze_failure inst.vhdl 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: analysis crashed instead of reporting a clean error"
  exit 1
fi

if ! echo "$out" | grep -q "form of expression for unbounded inertial"; then
  echo "FAIL: expected the unbounded-inertial-association error"
  exit 1
fi

# 2. The same port map with the concatenation of two *constants*, which is
#    globally static and therefore legal.  It used to fail on the gcc and
#    llvm backends and work on mcode; it works everywhere now.
analyze inst3.vhdl
elab_simulate inst3

# 3. The variant where the unconstrained formals are constrained by slices
#    in the port map itself, which is what the reporter tried next.  c_data
#    is sliced in two parts, so LRM08 5.3.2.2 e) 2) gives its index range
#    the direction of the index subtype of the base type ('to' for
#    natural), and the 'downto' slices of the port map are an error.  What
#    was wrong is what came after: GHDL raised CONSTRAINT_ERROR right after
#    printing the message, both when elaborating and under --synth.
#
#    The backends differ on when they report it (elaboration for mcode and
#    llvm-jit, run time for gcc and llvm), so check the message rather than
#    the exit status.
analyze inst2.vhdl

out=$(elab_simulate inst2 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed after reporting the slice direction error"
  exit 1
fi

if ! echo "$out" | grep -q "slice direction doesn't match"; then
  echo "FAIL: expected the slice direction error"
  exit 1
fi

out=$(synth inst2 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: --synth crashed after reporting the slice direction error"
  exit 1
fi

if ! echo "$out" | grep -q "slice direction doesn't match"; then
  echo "FAIL: expected the slice direction error from --synth"
  exit 1
fi

# 4. The same design with the multi-part formal written ascending, which is
#    the direction the rule gives it.  This is the shape that works.
analyze ok.vhdl
elab_simulate ok

synth ok > syn_ok.vhdl
rm -f syn_ok.vhdl

# 5. Slices that do not agree on a direction: same error, and again no
#    crash after it.
analyze bad.vhdl

out=$(elab_simulate bad 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: crashed after reporting the slice direction error"
  exit 1
fi

if ! echo "$out" | grep -q "slice direction doesn't match"; then
  echo "FAIL: expected the slice direction error"
  exit 1
fi

out=$(synth bad 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: --synth crashed after reporting the slice direction error"
  exit 1
fi

if ! echo "$out" | grep -q "slice direction doesn't match"; then
  echo "FAIL: expected the slice direction error from --synth"
  exit 1
fi

clean

echo "Test successful"
