#! /bin/sh

. ../../testenv.sh

# Test 1: counter.vhdl (we expect some warnings)
#
# We should get warnings about an incomplete and overspecified sensitivity
# list, so let's direct stderr to a file
analyze counter.vhdl 2> analyze.log
# Compare if our warnings match the expected warnings
diff_nocr analyze.log expected.txt

# Test 2: records.vhdl
# For this test and the rest, we expect no warnings, so fail if we get any
export GHDL_STD_FLAGS=-Werror
analyze records.vhdl

# Test 3: records_in_records.vhdl
analyze records_in_records.vhdl

# Test 4: arrays.vhdl
analyze arrays.vhdl

clean

echo "Test successful"
