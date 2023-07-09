#! /bin/sh

. ../../testenv.sh

# We should get warnings about an incomplete and overspecified sensitivity
# list, so let's direct stderr to a file
analyze counter.vhdl 2> analyze.log

# Compare if our warnings match the expected warnings
diff_nocr analyze.log expected.txt

clean

echo "Test successful"
