#! /bin/sh

# Stop in case of error
set -e

. ./testenv.sh

flag_log=yes
tests=

for opt; do
  case "$opt" in
      --nolog) flag_log=no ;;
      [a-z]*) tests="$tests $opt" ;;
      *) echo "$0: unknown option $opt"; exit 2 ;;
  esac
done

if [ x$tests = x ]; then tests="gna vests"; fi

printf "$ANSI_BLUE[$TASK| GHDL - test] Sourced the testsuite environment $ANSI_NOCOLOR\n"

# The GNA testsuite: regression testsuite using reports/issues from gna.org
do_gna() {
  cd gna
  if [ $flag_log = yes ]; then
      ./testsuite.sh 1>> ../../log.log 2>&1
  else
      ./testsuite.sh
  fi
  cd ..
}

# The VESTS testsuite: compliance testsuite, from: https://github.com/nickg/vests.git 388250486a
do_vests() {
  cd vests
  if [ $flag_log = yes ]; then
      ./testsuite.sh 1>> ../../log.log 2>&1
  else
      ./testsuite.sh
  fi
  cd ..
}

# Run a testsuite
do_test() {
  printf "$ANSI_BLUE[$TASK| GHDL - test] $1 $ANSI_NOCOLOR\n"
  case $1 in
      gna) do_gna;;
      vests) do_vests;;
      *)
          printf *e "$ANSI_RED$0: test name '$1' is unknown $ANSI_NOCOLOR"
          exit 1;;
  esac
}

printf "$ANSI_BLUE[$TASK| GHDL - test] GHDL is: $GHDL $ANSI_NOCOLOR\n"

for t in $tests; do do_test $t; done

printf "$ANSI_BLUE[$TASK| GHDL - test] $0:  $cGREENSuccess$ANSI_BLUE [$GHDL] $ANSI_NOCOLOR\n"
$GHDL --version 1>> ../log.log 2>&1
exit 0
