#! /bin/sh

. dist/ansi_color.sh
disable_color

# Stop in case of error
set -e

# Transform long options to short ones
for arg in "$@"; do
  shift
  case "$arg" in
      "--color"|"-color")   set -- "$@" "-c";;
      "--build"|"-build")   set -- "$@" "-b";;
      "--file"|"-file")     set -- "$@" "-f";;
      "--taskid"|"-taskid") set -- "$@" "-t";;
    *) set -- "$@" "$arg"
  esac
done
# Parse args
while getopts ":b:f:t:c" opt; do
  case $opt in
    c) enable_color;;
    b) BLD=$OPTARG ;;
    f) PKG_FILE=$OPTARG;;
    t) TASK=$OPTARG;;
    \?) printf "$ANSI_RED[GHDL] Invalid option: -$OPTARG $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
    :)  printf "$ANSI_RED[GHDL] Option -$OPTARG requires an argument. $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
  esac
done

#---

printf "$ANSI_BLUE[$TASK| GHDL] Prepare $(pwd) $ANSI_NOCOLOR\n"
CDIR=$(pwd)
prefix="$CDIR/install-$BLD"
mkdir "$prefix"
mkdir "build-$BLD"
cd "build-$BLD"

#--- Env

echo "travis_fold:start:env.$TASK"
printf "$ANSI_BLUE[$TASK| GHDL] Environment $ANSI_NOCOLOR\n"
env
echo "travis_fold:end:env.$TASK"

#--- Configure

echo "travis_fold:start:configure.$TASK"
printf "$ANSI_BLUE[$TASK| GHDL - build] Configure $ANSI_NOCOLOR\n"
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
	echo "$ANSI_RED[$TASK| GHDL - build] Unknown build $BLD $ANSI_NOCOLOR"
	exit 1;;
esac
echo "../configure --prefix=$prefix $config_opts"
../configure "--prefix=$prefix" $config_opts
echo "travis_fold:end:configure.$TASK"

#--- make

echo "travis_fold:start:make.$TASK"
printf "$ANSI_BLUE[$TASK| GHDL - build] Make $ANSI_NOCOLOR\n"
make
echo "travis_fold:end:make.$TASK"

echo "travis_fold:start:install.$TASK"
printf "$ANSI_BLUE[$TASK| GHDL - build] Install $ANSI_NOCOLOR\n"
make install
cd ..
echo "travis_fold:end:install.$TASK"

#--- package

echo "travis_fold:start:tar.$TASK"
printf "$ANSI_BLUE[$TASK| GHDL] Create package $ANSI_DARKCYAN$PKG_FILE $ANSI_NOCOLOR\n"
tar -zcvf "$PKG_FILE" -C "$prefix" .
echo "travis_fold:end:tar.$TASK"

#--- test

export ENABLECOLOR TASK
export GHDL="$prefix/bin/ghdl"
cd testsuite
failures=""

echo "travis_fold:start:tests.gna.$TASK"
printf "$ANSI_BLUE[$TASK| GHDL - test] gna $ANSI_NOCOLOR\n"
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
echo "travis_fold:end:tests.gna.$TASK"
[ "$failures" = "" ] || exit 1

echo "travis_fold:start:tests.vests.$TASK"
printf "$ANSI_BLUE[$TASK| GHDL - test] vests $ANSI_NOCOLOR\n"
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
echo "travis_fold:end:tests.vests.$TASK"
[ "$failures" = "" ] || exit 1

$GHDL --version
cd ..

#---

# Do not remove this line, and don't write anything below, since it is used to identify successful builds
echo "[$TASK|SUCCESSFUL]"
