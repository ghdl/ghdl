#! /bin/sh

# Stop in case of error.
set -e

# Source the testsuite environment
. ./testenv.sh

# The GNA testsuite:
# regression testsuite using reports/issues from gna.org
do_gna ()
{
  echo "**** GNA ****"
  echo "*************"
  cd gna
  ./testsuite.sh
  cd ..
}

# Run a testsuite
do_test ()
{
  case $1 in
      gna)
	  do_gna;;
      *)
          echo "$0: test name '$1' is unknown"
          exit 1;;
  esac
}

all_list="gna"

echo "GHDL is: $GHDL"

if [ $# -eq 0 ]; then
  for t in $all_list; do
    do_test $t
   done
else
  for t; do
    do_test $t
  done
fi

echo
echo "$0: Success ($GHDL)"
$GHDL --version
exit 0
