#! /bin/bash

scriptdir=$(dirname $0)

. "$scriptdir/utils.sh"
disable_color

echo "$0" "$@"

# Stop in case of error
set -e

export ISGPL=false

# Transform long options to short ones
for arg in "$@"; do
  shift
  case "$arg" in
      "--color"|"-color")   set -- "$@" "-c";;
      "--gpl"|"-gpl")       set -- "$@" "-g";;
    *) set -- "$@" "$arg"
  esac
done
# Parse args
while getopts ":b:p:cg" opt; do
  case $opt in
    c) enable_color;;
    g) ISGPL=true;;
    \?) printf "$ANSI_RED[GHDL - test] Invalid option: -$OPTARG $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
    :)  printf "$ANSI_RED[GHDL - test] Option -$OPTARG requires an argument. $ANSI_NOCOLOR\n" >&2
	exit 1 ;;
  esac
done

rm -f test_ok

export ENABLECOLOR
if [ "$GHDL" = "" ]; then
    export GHDL="$prefix/bin/ghdl"
fi
cd testsuite
failures=""

travis_start "tests.sanity" "$ANSI_YELLOW[GHDL - test] sanity $ANSI_NOCOLOR"
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
travis_finish "tests.sanity"
[ "$failures" = "" ] || exit 1

if [ "$ISGPL" != "true" ]; then
    travis_start "tests.gna" "$ANSI_YELLOW[GHDL - test] gna $ANSI_NOCOLOR"
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
    travis_finish "tests.gna"
    [ "$failures" = "" ] || exit 1
fi

travis_start "tests.vests" "$ANSI_YELLOW[GHDL - test] vests $ANSI_NOCOLOR"
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
travis_finish "tests.vests"
[ "$failures" = "" ] || exit 1

$GHDL --version
cd ..

#---

echo "[SUCCESSFUL]"
touch test_ok
