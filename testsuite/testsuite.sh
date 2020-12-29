#! /bin/bash

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

if [ -n "$TRAVIS" ]; then
  echo "INFO: set 'gstart' and 'gend' for TRAVIS"
  # This is a trimmed down copy of https://github.com/travis-ci/travis-build/blob/master/lib/travis/build/bash/*
  travis_time_start() {
    # `date +%N` returns the date in nanoseconds. It is used as a replacement for $RANDOM, which is only available in bash.
    travis_timer_id=`date +%N`
    travis_start_time=$(travis_nanoseconds)
    echo "travis_time:start:$travis_timer_id"
  }
  travis_time_finish() {
    travis_end_time=$(travis_nanoseconds)
    local duration=$(($travis_end_time-$travis_start_time))
    echo "travis_time:end:$travis_timer_id:start=$travis_start_time,finish=$travis_end_time,duration=$duration"
  }

  if [ "$TRAVIS_OS_NAME" = "osx" ]; then
    travis_nanoseconds() {
      date -u '+%s000000000'
    }
  else
    travis_nanoseconds() {
      date -u '+%s%N'
    }
  fi

  gstart () {
    echo "travis_fold:start:group"
    travis_time_start
    print_start "$@"
  }

  gend () {
    travis_time_finish
    echo "travis_fold:end:group"
  }
else
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
      printf "${ANSI_GRAY}took $(($duration / 60)) min $(($duration % 60)) sec.${ANSI_NOCOLOR}\n"
    }
  fi
fi

#---

do_sanity () {
  gstart "[GHDL - test] sanity"
  cd sanity

  for d in [0-9]*; do
    cd $d
    if ./testsuite.sh > test.log 2>&1 ; then
      printf "sanity $d: ${ANSI_GREEN}ok${ANSI_NOCOLOR}\n"
      # Don't disp log
    else
      printf "sanity $d: ${ANSI_RED}failed${ANSI_NOCOLOR}\n"
      cat test.log
      failures="$failures $d"
    fi
    cd ..
    # Stop at the first failure
    [ "$failures" = "" ] || break
  done

  cd ..
  gend
  [ "$failures" = "" ] || exit 1
}

# The GNA testsuite: regression testsuite using reports/issues from gna.org and from GitHub
do_gna () {
  gstart "[GHDL - test] gna"
  cd gna

  dirs=`./testsuite.sh --list-tests`
  for d in $dirs; do
    cd $d
    if ./testsuite.sh > test.log 2>&1 ; then
      printf "gna $d: ${ANSI_GREEN}ok${ANSI_NOCOLOR}\n"
      # Don't disp log
    else
      printf "gna $d: ${ANSI_RED}failed${ANSI_NOCOLOR}\n"
      cat test.log
      failures="$failures $d"
    fi
    cd ..
    # Stop at the first failure
    [ "$failures" = "" ] || break
  done

  cd ..
  gend
  [ "$failures" = "" ] || exit 1
}

# The Python Unit testsuite: regression testsuite for Python bindings to libghdl
do_pyunit () {
  gstart "[GHDL - test] pyunit"
  PYTHONPATH=$(pwd)/.. python3 -m pytest --forked pyunit
  gend
}

# The VESTS testsuite: compliance testsuite, from: https://github.com/nickg/vests.git 388250486a
do_vests () {
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

do_synth () {
  gstart "[GHDL - test] synth"
  cd synth

  if ./testsuite.sh > synth.log 2>&1 ; then
    printf "${ANSI_GREEN}Synth is OK$ANSI_NOCOLOR\n"
    wc -l synth.log
  else
    cat synth.log
    printf "${ANSI_RED}Synth failure$ANSI_NOCOLOR\n"
    failures="synth"
  fi

  cd ..
  gend
  [ "$failures" = "" ] || exit 1
}

#---

do_vpi () {
  gstart "[GHDL - test] vpi"
  cd vpi

  for d in *[0-9]; do
    cd $d
    if ./testsuite.sh > test.log 2>&1 ; then
      printf "vpi $d: ${ANSI_GREEN}ok${ANSI_NOCOLOR}\n"
      # Don't disp log
    else
      printf "vpi $d: ${ANSI_RED}failed${ANSI_NOCOLOR}\n"
      cat test.log
      failures="$failures $d"
    fi
    cd ..
    # Stop at the first failure
    [ "$failures" = "" ] || break
  done

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
    printf "${ANSI_RED}error: GHDL environment variable is not defined${ANSI_NOCOLOR}\n"
    exit 1
  fi
fi

cd $(dirname $0)
rm -f test_ok
failures=""
tests=

for opt; do
  case "$opt" in
      [a-z]*) tests="$tests $opt" ;;
      *) echo "$0: unknown option $opt"; exit 2 ;;
  esac
done

if [ "x$tests" = "x" ]; then tests="sanity pyunit gna vests synth vpi"; fi

echo "tests: $tests"

# Run a testsuite
do_test() {
  case $1 in
    sanity) do_sanity;;
    pyunit) do_pyunit;;
    gna)    do_gna;;
    vests)  do_vests;;
    synth)  do_synth;;
    vpi)    do_vpi;;
    *)
      printf "${ANSI_RED}$0: test name '$1' is unknown${ANSI_NOCOLOR}\n"
      exit 1;;
  esac
}

for t in $tests; do do_test $t; done

printf "${ANSI_GREEN}[GHDL - test] SUCCESSFUL${ANSI_NOCOLOR}\n"
touch test_ok

gstart "GHDL is: $GHDL"
$GHDL version
gend

gstart "GHDL help"
$GHDL help
gend
