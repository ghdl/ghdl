#! /bin/sh

. ../../testenv.sh

# A recursive function operating on a nested record-containing-
# unconstrained-array type (state_array_t -> state_t -> value_array_t)
# used to crash the gcc-backend translator with an internal
# ASSERT_FAILURE (trans-chap3.adb:60). See issue1918.
analyze --std=08 -frelaxed-rules test_pkg.vhd

clean

echo "Test successful"
