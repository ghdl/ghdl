#! /bin/sh

# Stop in case of error
set -e

. ./testenv.sh

tests=

for opt; do
  case "$opt" in
      [a-z]*) tests="$tests $opt" ;;
      *) echo "$0: unknown option $opt"; exit 2 ;;
  esac
done

if [ x$tests = x ]; then tests="gna vests"; fi

# The GNA testsuite: regression testsuite using reports/issues from gna.org
do_gna() {
  cd gna
  ./testsuite.sh
  cd ..
}

# The VESTS testsuite: compliance testsuite, from: https://github.com/nickg/vests.git 388250486a
do_vests() {
  cd vests
  ./testsuite.sh
  cd ..
}

# Run a testsuite
do_test() {
  case $1 in
      gna) do_gna;;
      vests) do_vests;;
      *)
          echo "$0: test name '$1' is unknown"
          exit 1;;
  esac
}


for t in $tests; do do_test $t; done

echo "$0: Success"

echo "GHDL is: $GHDL"
$GHDL --version

exit 0
