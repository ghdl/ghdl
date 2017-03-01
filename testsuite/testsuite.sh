#! /bin/sh

# Stop in case of error
set -e

. ./testenv.sh
printf "$ANSI_BLUE[$TASK| GHDL - test] Sourced the testsuite environment $ANSI_NOCOLOR\n"

# The GNA testsuite: regression testsuite using reports/issues from gna.org
do_gna() {
  cd gna && ./testsuite.sh 1>> ../../log.log 2>&1
  cd ..
}

# The VESTS testsuite: compliance testsuite, from: https://github.com/nickg/vests.git 388250486a
do_vests() {
  gnatmake get_entities 1>> ../log.log 2>&1
  cd vests && ./testsuite.sh
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

if [ $# -eq 0 ]; then tests="gna vests";
else tests=$("$@"); fi

for t in $tests; do do_test $t; done

printf "$ANSI_BLUE[$TASK| GHDL - test] $0:  $cGREENSuccess$ANSI_BLUE [$GHDL] $ANSI_NOCOLOR\n"
$GHDL --version 1>> ../log.log 2>&1
exit 0
