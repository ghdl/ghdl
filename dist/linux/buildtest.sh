#! /bin/bash

. dist/ansi_color.sh
disable_color

echo "$0" "$@"

# Stop in case of error
set -e

. dist/linux/travis-utils.sh

rm -f build_ok

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
    \?) printf "$ANSI_RED[GHDL] Invalid option: -$OPTARG $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
    :)  printf "$ANSI_RED[GHDL] Option -$OPTARG requires an argument. $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
  esac
done

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
printf "$ANSI_YELLOW[GHDL] Configure $ANSI_NOCOLOR\n"

CDIR=$(pwd)
prefix="$CDIR/install-$BLD"
mkdir "$prefix"
mkdir "build-$BLD"
cd "build-$BLD"

case "$BLD" in
    mcode)
	config_opts="" ;;
    llvm)
	config_opts="--with-llvm-config" ;;
    llvm-3.5)
	config_opts="--with-llvm-config=llvm-config-3.5 CXX=clang++" ;;
    llvm-3.8)
	config_opts="--with-llvm-config=llvm-config-3.8 CXX=clang++-3.8" ;;
    docker)
	echo "Check docker container!"
	exit 0;;
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
printf "$ANSI_YELLOW[GHDL] Make $ANSI_NOCOLOR\n"
make
travis_time_finish
echo "travis_fold:end:make"

echo "travis_fold:start:install"
printf "$ANSI_YELLOW[GHDL] Install $ANSI_NOCOLOR\n"
make install
cd ..
echo "travis_fold:end:install"

#--- package

echo "travis_fold:start:tar.bin"
printf "$ANSI_YELLOW[GHDL] Create package ${ANSI_DARKCYAN}${PKG_NAME}.tgz $ANSI_NOCOLOR\n"
tar -zcvf "${PKG_NAME}.tgz" -C "$prefix" .
echo "travis_fold:end:tar.bin"

#--- test

export ENABLECOLOR TASK
export GHDL="$prefix/bin/ghdl"
cd testsuite
failures=""

echo "travis_fold:start:tests.sanity"
travis_time_start
printf "$ANSI_YELLOW[Test] sanity $ANSI_NOCOLOR\n"
cd sanity
for d in [0-9]*; do
    cd $d
    if ./testsuite.sh > test.log 2>&1 ; then
	echo "sanity $d: ok"
	# Don't disp log
    else
	echo "${ANSI_RED}sanity $d: failed${ANSI_NOCOLOR}"
	cat test.log
	failures="$failures $d"
    fi
    cd ..
    # Stop at the first failure
    [ "$failures" = "" ] || break
done
cd ..
travis_time_finish
echo "travis_fold:end:tests.sanity"
[ "$failures" = "" ] || exit 1

if [ "$ISGPL" != "true" ]; then
    echo "travis_fold:start:tests.gna"
    travis_time_start
    printf "$ANSI_YELLOW[Test] gna $ANSI_NOCOLOR\n"
    cd gna
    dirs=`./testsuite.sh --list-tests`
    for d in $dirs; do
	cd $d
	if ./testsuite.sh > test.log 2>&1 ; then
	    echo "gna $d: ok"
	    # Don't disp log
	else
	    echo "${ANSI_RED}gna $d: failed${ANSI_NOCOLOR}"
	    cat test.log
	    failures="$failures $d"
	fi
	cd ..
	# Stop at the first failure
	[ "$failures" = "" ] || break
    done
    cd ..
    travis_time_finish
    echo "travis_fold:end:tests.gna"
    [ "$failures" = "" ] || exit 1
fi

echo "travis_fold:start:tests.vests"
travis_time_start
printf "$ANSI_YELLOW[Test] vests $ANSI_NOCOLOR\n"
cd vests
if ./testsuite.sh > vests.log 2>&1 ; then
    echo "${ANSI_GREEN}Vests is OK$ANSI_NOCOLOR"
    wc -l vests.log
else
    cat vests.log
    echo "${ANSI_RED}Vests failure$ANSI_NOCOLOR"
    failures=vests
fi
cd ..
travis_time_finish
echo "travis_fold:end:tests.vests"
[ "$failures" = "" ] || exit 1

$GHDL --version
cd ..

#---

echo "[SUCCESSFUL]"
touch build_ok
