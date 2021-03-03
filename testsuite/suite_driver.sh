#! /bin/sh

# Driver for a testsuite
# The first positional argument is required, it's the name of the suite to be executed

set -e

ANSI_GREEN="\033[32m"
ANSI_RED="\033[31m"
ANSI_NOCOLOR="\033[0m"

_suite="$1"
shift

# This is the only place where test dirs are specified.
#Do not duplicate this line
dirs="*[0-9]*"

failures=""
full=n

for opt; do
  case "$opt" in
  -k | --keep-going)  full=y ;;
  --dir=*) dirs="$(echo "$opt" | sed -e 's/--dir=//')" ;;
  --skip=*) d="$(echo "$opt" | sed -e 's/--skip=//')"
            dirs="$(echo "" "$dirs" | sed -e "s/ $d//")" ;;
  --start-at=*) d="$(echo "$opt" | sed -e 's/--start-at=//')"
            dirs="$(echo "" "$dirs" | sed -e "s/^.* $d//")"
            dirs="$d $dirs" ;;
  --list-tests) echo "$dirs"; exit 0;;
  *) echo "Unknown option $opt"
     exit 2
     ;;
  esac
done

singlerun() {
  cd "$1"
  if ./testsuite.sh > test.log 2>&1 ; then
    printf "$_suite $1: ${ANSI_GREEN}ok${ANSI_NOCOLOR}\n"
    # Don't disp log
  else
    printf "$_suite $1: ${ANSI_RED}failed${ANSI_NOCOLOR}\n"
    cat test.log
    if [ x"$2" = x"y" ]; then
      failures="$failures $1"
    else
      exit 1;
    fi
  fi
  cd ..
}

for i in $dirs; do singlerun "$i" "$full"; done

if [ x"$failures" = x"" ]; then
    echo "$_suite tests are successful" && exit 0
else
    echo "$_suite test failed ($failures)" && exit 1
fi
