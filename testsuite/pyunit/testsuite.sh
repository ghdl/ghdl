#! /bin/sh

# Driver for a testsuite.

set -e

# This is the only place where test dirs are specified.  Do not duplicate this
# line
#dirs="*[0-9]"
#
#failures=""
#full=n
#
#for opt; do
#  case "$opt" in
#  -k | --keep-going)  full=y ;;
#  --dir=*) dirs=`echo $opt | sed -e 's/--dir=//'` ;;
#  --skip=*) d=`echo $opt | sed -e 's/--skip=//'`
#            dirs=`echo "" $dirs | sed -e "s/ $d//"` ;;
#  --start-at=*) d=`echo $opt | sed -e 's/--start-at=//'`
#            dirs=`echo "" $dirs | sed -e "s/^.* $d//"`
#            dirs="$d $dirs" ;;
#  --list-tests) echo $dirs; exit 0;;
#  *) echo "Unknown option $opt"
#     exit 2
#     ;;
#  esac
#done
#
#singlerun() {
#  echo ""
#  echo "dir $1:"
#  cd $1
#  if ! ./testsuite.sh; then
#    echo "#################################################################"
#    echo "######### FAILURE: $1"
#    echo "#################################################################"
#    if [ $2 = "y" ]; then
#      failures="$failures $1"
#    else
#      exit 1;
#    fi
#  fi
#  cd ..
#}
#
#for i in $dirs; do singlerun $i $full; done
#
#if [ x"$failures" = x"" ]; then
#    echo "tests are successful" && exit 0
#else
#    echo "test failed ($failures)" && exit 1
#fi

cd $(dirname "$0")/../..

python3 -m unittest testsuite.pyunit
