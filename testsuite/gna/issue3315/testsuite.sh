#! /bin/sh

. ../../testenv.sh

# A formal generic package (local_pkg) whose declaration has an explicit
# (non "<>") generic map aspect, matched against an actual package
# instantiation, hit an unimplemented case in Sem_Association_Package
# that crashed with a raw internal error instead of a diagnostic. See
# issue3315.
export GHDL_STD_FLAGS="--std=08"

# ram.vhdl and test_ram.vhdl must be analyzed as two separate GHDL
# invocations, not as one `analyze_failure ram.vhdl test_ram.vhdl` call:
# with the GCC/LLVM backends, `ghdl -a` spawns one ghdl1 subprocess per
# file and forwards --expect-failure to each of them individually, so a
# clean file given alongside a failing one (like ram.vhdl here) would
# itself be flagged as an unexpected non-failure and abort the whole
# command before test_ram.vhdl is even analyzed. mcode analyzes all files
# in one process and only checks the overall error count, so it doesn't
# hit this. See issue.md for the full analysis.
analyze ram.vhdl

out=$(analyze_failure test_ram.vhdl 2>&1)
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: analysis crashed instead of reporting a clean error"
  exit 1
fi

if ! echo "$out" | grep -q "matching a formal generic package with an explicit generic map aspect is not supported"; then
  echo "FAIL: expected the not-supported diagnostic"
  exit 1
fi

clean

echo "Test successful"
