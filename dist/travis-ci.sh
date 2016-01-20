#! /bin/sh
# This script is executed in the travis-ci environment.

# Stop in case of error
set -e

CDIR=$PWD

# Prepare
prefix="$CDIR/install-$1"
mkdir "$prefix"
mkdir build-$1
cd build-$1

# Configure
case "$1" in
  mcode)
    ../configure --prefix="$prefix" ;;

  llvm)
    ../configure --prefix="$prefix" --with-llvm-config=llvm-config-3.5 ;;

  *)
    echo "unknown build $1"
    exit 1
    ;;
esac

# Build
make
make install
cd ..

# Test
export GHDL="$CDIR/install-$1/bin/ghdl"
cd testsuite
gnatmake get_entities
./testsuite.sh
cd ..
