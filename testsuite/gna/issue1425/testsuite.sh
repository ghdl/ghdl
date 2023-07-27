#! /bin/sh

. ../../testenv.sh

# Test 1: counter.vhdl (we expect some warnings)
#
# We should get warnings about an incomplete and overspecified sensitivity
# list, so let's direct stderr to a file
analyze counter.vhdl 2> analyze.log
# Compare if our warnings match the expected warnings
diff_nocr analyze.log expected.txt

# Test 2: records.vhdl (we expect no warnings, so fail if we get any)
GHDL_STD_FLAGS=-Werror
analyze records.vhdl

# Test 3: records_in_records.vhdl (also we expect no warnings)
GHDL_STD_FLAGS=-Werror
analyze records_in_records.vhdl

clean

echo "Test successful"
