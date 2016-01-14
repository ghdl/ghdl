#! /bin/sh
# This script is executed in the travis-ci environment.

# Stop in case of error
set -e

CDIR=$PWD

# Build mcode64
mkdir build-mcode64
mkdir install-mcode64
cd build-mcode64
../configure --prefix=$CDIR/install-mcode64
make
make install
cd ..

# Test mcode64
export GHDL=$CDIR/install-mcode64/bin/ghdl
cd testsuite
gnatmake get_entities
./testsuite.sh
cd ..

# build for llvm
mkdir build-llvm
mkdir install-llvm
cd build-llvm
../configure --prefix=$CDIR/install-llvm --with-llvm-config=llvm-config-3.5
make
make install
cd ..

# Test llvm
export GHDL=$CDIR/install-llvm/bin/ghdl
cd testsuite
./testsuite.sh
cd ..
