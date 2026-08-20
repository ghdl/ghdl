#! /bin/sh

. ../../testenv.sh

# A protected-type variable used in a process (inherently not
# synthesizable hardware) used to crash --synth with an internal
# TYPES.INTERNAL_ERROR ("synth_concurrent_declaration: cannot handle
# IIR_KIND_PROTECTED_TYPE_BODY") instead of reporting a clean
# unsupported-construct error. See issue2037.
export GHDL_STD_FLAGS="--std=08"

analyze --work=ti crc_pkg.vhd
analyze crc_entity_draft.vhd

out=$(synth --out=verilog crc_entity_draft 2>&1) || true
echo "$out"

if echo "$out" | grep -q "GHDL Bug occurred"; then
  echo "FAIL: synth crashed instead of reporting a clean error"
  exit 1
fi

if ! echo "$out" | grep -q "protected type variable is not synthesizable"; then
  echo "FAIL: expected the protected-type-not-synthesizable error"
  exit 1
fi

clean ti
clean

echo "Test successful"
