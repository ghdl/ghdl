#! /usr/bin/env bash

# Stop in case of error
set -e

enable_color() {
  ENABLECOLOR=''
  ANSI_RED="\033[31m"
  ANSI_GREEN="\033[32m"
  ANSI_YELLOW="\033[33m"
  ANSI_BLUE="\033[34m"
  ANSI_MAGENTA="\033[35m"
  ANSI_GRAY="\033[90m"
  ANSI_CYAN="\033[36;1m"
  ANSI_DARKCYAN="\033[36m"
  ANSI_NOCOLOR="\033[0m"
}

disable_color() { unset ENABLECOLOR ANSI_RED ANSI_GREEN ANSI_YELLOW ANSI_BLUE ANSI_MAGENTA ANSI_CYAN ANSI_DARKCYAN ANSI_NOCOLOR; }
enable_color

die() {
  printf "${ANSI_RED}%s${ANSI_NOCOLOR}\n" "$@" >&2
  exit 1
}

print_start() {
  COL="$ANSI_YELLOW"
  if [ "x$2" != "x" ]; then
    COL="$2"
  fi
  printf "${COL}${1}$ANSI_NOCOLOR\n"
}

gstart () {
  print_start "$@"
}
gend () {
  :
}

if [ -n "$CI" ]; then
  echo "INFO: set 'gstart' and 'gend' for CI"
  gstart () {
    printf '::group::'
    print_start "$@"
    SECONDS=0
  }

  gend () {
    duration=$SECONDS
    echo '::endgroup::'
    printf "${ANSI_GRAY}took $((duration / 60)) min $((duration % 60)) sec.${ANSI_NOCOLOR}\n"
  }
fi

#---

# The VESTS testsuite: compliance testsuite, from: https://github.com/nickg/vests.git 388250486a
_vests () {
  gstart "[GHDL - test] vests"
  cd vests

  if ./testsuite.sh > vests.log 2>&1 ; then
    printf "${ANSI_GREEN}Vests is OK$ANSI_NOCOLOR\n"
    wc -l vests.log
  else
    cat vests.log
    printf "${ANSI_RED}Vests failure$ANSI_NOCOLOR\n"
    failures=vests
  fi

  cd ..
  gend
  [ "$failures" = "" ] || exit 1
}

#---

if [ "x$GHDL" = "x" ]; then
  if [ "x$prefix" != "x" ]; then
    export GHDL="$prefix/bin/ghdl"
  elif [ "x$(command -v which)" != "x" ]; then
    export GHDL="$(which ghdl)"
  else
    die "error: GHDL environment variable is not defined"
  fi
fi

if [ "$GHWDUMP" = "" ]; then
  case "$GHDL" in
    */*) export GHWDUMP=${GHDL%/*}/ghwdump;;
    *) export GHWDUMP=ghwdump;;
  esac
fi

command -v "$GHWDUMP" >/dev/null || die "ghwdump executable not found: $GHWDUMP"

cd $(dirname "$0")
rm -f test_ok
failures=""
tests=

for opt; do
  shift
  case "$opt" in
      [a-z]*) tests="$tests $opt" ;;
      --) break ;;
      *) echo "$0: unknown option $opt"; exit 2 ;;
  esac
done

if [ "x$tests" = "x" ]; then tests="sanity pyunit gna vests synth vpi vhpi"; fi

echo "> tests: $tests"
echo "> args: $@"

# Run a testsuite
do_test() {
  case $1 in
    sanity|gna|synth|vpi|vhpi)
      gstart "[GHDL - test] $1"
      cd "$1"
      ../suite_driver.sh $@
      cd ..
      gend
      [ "$failures" = "" ] || exit 1
    ;;

    pyunit)
      # The Python Unit testsuite: regression testsuite for Python bindings to libghdl
      gstart "[GHDL - test] pyunit"
      PYTHONPATH=$(pwd)/.. ${PYTHON:-python3} -m pytest -vsrA pyunit
      gend
    ;;

    vests)
      _vests
    ;;
    *)
      die "$0: test name '$1' is unknown"
    ;;
  esac
}

gstart "GHDL is: $GHDL"
$GHDL version
echo "REF: $($GHDL version ref)"
echo "HASH: $($GHDL version hash)"
gend

gstart "GHDL help"
$GHDL help
gend

for t in $tests; do do_test "$t"; done

printf "${ANSI_GREEN}[GHDL - test] SUCCESSFUL${ANSI_NOCOLOR}\n"
touch test_ok
