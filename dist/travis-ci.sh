#! /bin/sh
# This script is executed in the travis-ci environment.

# Stop in case of error
set -e

CDIR=$PWD
BLD=$1

# Display environment
echo "Environment:"
env

# Prepare
prefix="$CDIR/install-$BLD"
mkdir "$prefix"
mkdir build-$BLD
cd build-$BLD

# Configure
case "$BLD" in
  mcode)
      ../configure --prefix="$prefix"
      MAKEOPTS=""
      ;;

  llvm*)
      llvm_ver=`echo $BLD | sed -e 's/llvm//'`
      ../configure --prefix="$prefix" --with-llvm-config=llvm-config$llvm_ver
      MAKEOPTS="CXX=clang++$llvm_ver"
      ;;

  *)
      echo "unknown build $BLD"
      exit 1
      ;;
esac

# Build
make $MAKEOPTS
make install
cd ..

# Package
PKG_VER=`grep Ghdl_Ver src/version.ads | sed -e 's/.*"\(.*\)";/\1/'`

if [ "$TRAVIS_TAG" = "" ]; then
    PKG_TAG=`date -u +%Y%m%d`
else
    PKG_TAG=$TRAVIS_TAG
fi
PKG_FILE=ghdl-$PKG_VER-$BLD-$PKG_TAG.tgz
echo "creating $PKG_FILE"
tar -zcvf $PKG_FILE -C $prefix .

# Test
export GHDL="$CDIR/install-$1/bin/ghdl"
cd testsuite
gnatmake get_entities
./testsuite.sh
cd ..
