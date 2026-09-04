#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

# The original report: a package with a recursive to_string over an array of
# records whose elements are unbounded (sfixed).
analyze test_pkg.vhdl

# The reduction posted on the issue: a constant of a nested unconstrained
# array type, whose element subtype is only known from the aggregate.
analyze crash.vhdl
elab_simulate crashing_entity

clean

echo "Test successful"
