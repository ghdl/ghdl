#! /bin/bash

. dist/linux/travis-utils.sh
. dist/ansi_color.sh
disable_color

echo "$0" "$@"

# Stop in case of error
set -e

# Transform long options to short ones
for arg in "$@"; do
  shift
  case "$arg" in
      "--color"|"-color")   set -- "$@" "-c";;
      "--build"|"-build")   set -- "$@" "-b";;
      "--pkg"|"-pkg")       set -- "$@" "-p";;
      "--gpl"|"-gpl")       set -- "$@" "-g";;
    *) set -- "$@" "$arg"
  esac
done
# Parse args
while getopts ":b:p:cg" opt; do
  case $opt in
    c) enable_color;;
    b) BLD=$OPTARG ;;
    p) PKG_NAME=$OPTARG;;
    g) ISGPL=true;;
    \?) printf "$ANSI_RED[GHDL - build] Invalid option: -$OPTARG $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
    :)  printf "$ANSI_RED[GHDL - build] Option -$OPTARG requires an argument. $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
  esac
done

rm -f build_ok

#--- Env

echo "travis_fold:start:env.docker"
printf "$ANSI_YELLOW[Info] Environment $ANSI_NOCOLOR\n"
env
echo "travis_fold:end:env.docker"

#--- GPL: gpl-ize sources

if [ "$ISGPL" = "true" ]; then
    echo "travis_fold:start:gpl.src"
    printf "$ANSI_YELLOW[Source] create GPL sources $ANSI_NOCOLOR\n"
    files=`echo *`
    make -f Makefile.in srcdir=. clean-pure-gpl
    mkdir ${PKG_NAME}
    cp -pdrl $files ${PKG_NAME}
    tar -zcf "${PKG_NAME}.tar.gz" ${PKG_NAME}
    PKG_NAME="${PKG_NAME}-${BLD}"
    echo "travis_fold:end:gpl.src"
fi

#--- Configure

echo "travis_fold:start:configure"
printf "$ANSI_YELLOW[GHDL - build] Configure $ANSI_NOCOLOR\n"

CDIR=$(pwd)
export prefix="$CDIR/install-$BLD"
mkdir "$prefix"
mkdir "build-$BLD"
cd "build-$BLD"

case "$BLD" in
    mcode)
	      config_opts=""
        CXX=""
    ;;
    llvm)
	      CXX="clang"
	      config_opts="--with-llvm-config CXX=$CXX"
    ;;
    llvm-3.5)
	      CXX="clang++"
	      config_opts="--with-llvm-config=llvm-config-3.5 CXX=$CXX"
    ;;
    llvm-3.8)
	      CXX="clang++-3.8"
	      config_opts="--with-llvm-config=llvm-config-3.8 CXX=$CXX"
    ;;
    llvm-3.9)
	      CXX="clang++-3.9"
	      config_opts="--with-llvm-config=llvm-config-3.9 CXX=$CXX"
    ;;
    llvm-4.0)
	      CXX="clang++-4.0"
	      config_opts="--with-llvm-config=llvm-config-4.0 CXX=$CXX"
    ;;
    llvm-5.0)
	      CXX="clang++-5.0"
	      config_opts="--with-llvm-config=llvm-config-5.0 CXX=$CXX"
    ;;
    *)
	      echo "$ANSI_RED[GHDL - build] Unknown build $BLD $ANSI_NOCOLOR"
	      exit 1;;
esac
echo "../configure --prefix=$prefix $config_opts"
../configure "--prefix=$prefix" $config_opts
echo "travis_fold:end:configure"

#--- make

echo "travis_fold:start:make"
travis_time_start
printf "$ANSI_YELLOW[GHDL - build] Make $ANSI_NOCOLOR\n"
make
travis_time_finish
echo "travis_fold:end:make"

echo "travis_fold:start:install"
printf "$ANSI_YELLOW[GHDL - build] Install $ANSI_NOCOLOR\n"
make install
cd ..
echo "travis_fold:end:install"

#--- package

echo "travis_fold:start:tar.bin"
printf "$ANSI_YELLOW[GHDL - build] Create package ${ANSI_DARKCYAN}${PKG_NAME}.tgz $ANSI_NOCOLOR\n"
tar -zcvf "${PKG_NAME}.tgz" -C "$prefix" .
echo "travis_fold:end:tar.bin"

#--- build tools versions

{
    make --version | grep 'Make'
    gnatls --version | grep 'GNATLS'
    gcc --version | grep 'gcc'
    if [ "$CXX" != "" ]; then
	$CXX --version | grep 'clang'
    fi
} > BUILD_TOOLS

#---

echo "[SUCCESSFUL]"
touch build_ok
