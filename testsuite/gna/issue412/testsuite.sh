#! /bin/sh

. ../../testenv.sh

# An entity with a fully generic (unconstrained) "type" generic used to
# crash the gcc-backend translator with an internal CONSTRAINT_ERROR
# (trans.adb:1426 access check failed). See issue412.
analyze --std=08 test.vhdl

clean

echo "Test successful"
