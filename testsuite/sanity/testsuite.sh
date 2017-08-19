#! /bin/sh

# Driver for sanity checks

set -e

dirs="[0-9]*"

failures=""
full=n

for opt; do
  case "$opt" in
  -k | --keep-going)  full=y ;;
  --list-tests) echo $dirs; exit 0;;
  *) echo "Unknown option $opt"
     exit 2
     ;;
  esac
done

singlerun() {
  echo ""
  echo "sanity $1:"
  cd $1
  if ! ./testsuite.sh; then
    echo "#################################################################"
    echo "######### FAILURE: $1"
    echo "#################################################################"
    if [ $2 = "y" ]; then
      failures="$failures $1"
    else
      exit 1;
    fi
  fi
  cd ..
}

for i in $dirs; do singlerun $i $full; done

if [ x"$failures" = x"" ]; then
    echo "sanity tests are successful" && exit 0
else
    echo "sanity test failed ($failures)" && exit 1
fi
