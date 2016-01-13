#! /bin/sh
# This script is executed in the travis-ci environment.

# Stop in case of error
set -e

# Build
./configure
make

# Test
export GHDL=$PWD/ghdl_mcode
cd testsuite
gnatmake get_entities
./testsuite.sh
