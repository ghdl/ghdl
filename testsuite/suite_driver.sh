#! /bin/sh

# Driver for a testsuite
# The first positional argument is required, it's the name of the suite to be executed

set -e

ANSI_GREEN="\033[32m"
ANSI_RED="\033[31m"
ANSI_NOCOLOR="\033[0m"

parse_cmdline () {
  _suite="$1"
  shift

  # This is the only place where test dirs are specified.
  #Do not duplicate this line
  dirs="*[0-9]*"

  full=n

  for opt; do
    case "$opt" in
    -k | --keep-going)  full=y ;;
    -j*) NPROC=${opt#-j} ;;
    --dir=*) dirs="$(echo "$opt" | sed -e 's/--dir=//')" ;;
    --skip=*) d="$(echo "$opt" | sed -e 's/--skip=//')"
              dirs="$(echo "" "$dirs" | sed -e "s/ $d//")" ;;
    --start-at=*) d="$(echo "$opt" | sed -e 's/--start-at=//')"
		  dirs="$(echo "" "$dirs" | sed -e "s/^.* $d//")"
		  dirs="$d $dirs" ;;
    --list-tests) echo $dirs; exit 0;;
    *) echo "Unknown option $opt"
       exit 2
       ;;
    esac
  done

  NPROC=${NPROC:-$(nproc 2> /dev/null || sysctl -n hw.ncpu 2> /dev/null || echo 1)}
}

singlerun() {
  cd "$1"
  if ./testsuite.sh > test.log 2>&1 ; then
    printf "$_suite $1: ${ANSI_GREEN}ok${ANSI_NOCOLOR}\n"
    # Don't disp log
  else
    printf "$_suite $1: ${ANSI_RED}failed${ANSI_NOCOLOR}\n"
    printf '%s ' "$1" >> ../failures.log
    if [ x"$2" = x"n" ]; then
      cat test.log
      exit 1;
    fi
  fi
  cd ..
}

allrun () {
  printf '' > failures.log

  if command -v xargs >/dev/null 2>&1 && [ "$NPROC" != 1 ]; then
    echo "..Running with $NPROC test workers.">&2
    ndirs=$(printf '%s\n' $dirs | wc -l)
    echo $dirs | DO_ALLRUN=0 xargs -P"$NPROC" -n$((1 + ndirs / NPROC)) sh -c \
      's=$1; _suite=$2 full=$3; shift 3; . "$s";
       for i in "$@"; do singlerun "$i" "$full" || true; done' \
      \
      sh "$0" "$_suite" "$full" || true
  else
    for i in $dirs; do singlerun "$i" "$full"; done
  fi

  if [ ! -e failures.log ]; then
    echo "error: Couldn't find test driver generated failures.log!">&2
    exit 1
  fi

  failures="$(cat failures.log)"
  if [ x"$failures" = x"" ]; then
    echo "$_suite tests are successful" && exit 0
  else
    for failed in $failures; do
      printf "$_suite $failed: ${ANSI_RED}failed${ANSI_NOCOLOR}\n"
      cat "$failed"/test.log
      printf '\n\n'
    done

    echo "$_suite test failed ($failures)" && exit 1
  fi
}

if [ "$DO_ALLRUN" != 0 ]; then
  parse_cmdline "$@"
  allrun
fi
