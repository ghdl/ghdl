#! /bin/sh

. ../../testenv.sh

# A partial (bit 0 excluded) write to a memory word used to crash
# --synth with an internal ASSERT_FAILURE (netlists-builders.adb:1266)
# instead of splitting the memory by write pattern. See issue1636.
synth asym_test.vhd -e asym_test > syn_asym_test.vhd
analyze syn_asym_test.vhd

clean

echo "Test successful"
