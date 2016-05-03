#! /bin/sh
# This script is executed in the travis-ci environment.

# Stop in case of error
set -e

CDIR=$PWD
BLD=$1

# Prepare
prefix="$CDIR/install-$BLD"
mkdir "$prefix"
mkdir build-$BLD
cd build-$BLD

# Configure
case "$BLD" in
  mcode)
    ../configure --prefix="$prefix" ;;

  llvm)
    ../configure --prefix="$prefix" --with-llvm-config=llvm-config-3.5 ;;

  *)
    echo "unknown build $BLD"
    exit 1
    ;;
esac

# Build
make
make install
cd ..

# Package
PKG_VER=`grep Ghdl_Ver src/version.ads | sed -e 's/.*"\(.*\)";/\1/'`

if [ "$TRAVIS_TAG" = "x" ]; then
    PKG_TAG=`date -u +%Y%m%d`
else
    PKG_TAG=$TRAVIS_TAG
fi
tar -zcvf ghdl-$PKG_VER-$BLD-$PKG_TAG.tgz -C $prefix .

# Test
export GHDL="$CDIR/install-$1/bin/ghdl"
cd testsuite
gnatmake get_entities
echo -e "\e[0;31mPatrick Lehmann: disabled testsuite to reduce the log while testing deployment to GitHub releases.\e[0m"
# ./testsuite.sh
cd ..
