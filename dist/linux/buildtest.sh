#! /bin/sh
# This script is executed in the travis-ci environment.

# Stop in case of error
set -e

while getopts ":b:f:" opt; do
  case $opt in
    b) BLD=$OPTARG ;;
	f) PKG_FILE=$OPTARG;;
    \?) echo "Invalid option: -$OPTARG" >&2; exit 1 ;;
    :)  echo "Option -$OPTARG requires an argument." >&2; exit 1 ;;
  esac
done

CDIR=$(pwd)

# Display environment
echo "Environment:"
env

# Prepare
prefix="$CDIR/install-$BLD"
mkdir "$prefix"
mkdir "build-$BLD"
cd "build-$BLD"

# Configure
case "$BLD" in
  mcode)
      ../configure --prefix="$prefix"
      MAKEOPTS=""
      ;;
	  
  llvm)
      ../configure --prefix="$prefix$" --with-llvm-config
      ;;
	  
  llvm-3.5)
      ../configure --prefix="$prefix" --with-llvm-config=llvm-config-3.5
      MAKEOPTS="CXX=clang++"
      ;;

  llvm-3.8)
      ../configure --prefix="$prefix" --with-llvm-config=llvm-config-3.8
      MAKEOPTS="CXX=clang++-3.8"
      ;;
	  
  docker) echo "Check docker container!"; exit 0;;

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
echo "creating $PKG_FILE"
tar -zcvf "$PKG_FILE" -C "$prefix" .

# Test
export GHDL="$CDIR/install-$BLD/bin/ghdl"
cd testsuite
gnatmake get_entities
./testsuite.sh
cd ..