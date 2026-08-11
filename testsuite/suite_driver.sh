#! /usr/bin/env bash

# Driver for a testsuite
# The first positional argument is required, it's the name of the suite to be executed

set -e

ANSI_GREEN=$'\x1b[32m'
ANSI_RED=$'\x1b[31m'
ANSI_NOCOLOR=$'\x1b[0m'

# suite_driver.sh runs with the suite's directory as its working directory, both when testsuite.sh starts it
# and when xargs starts a worker, so the shared helpers are one level up.
. ../lib.sh

# Parse command line options
# $1 - Testsuite name (testsuite directory)
# $@ - Further options
#
# Options:
# -k    --keep-going     - continue on error
# -j<N>                  - execute multiple tests in <N> parallel jobs
#       --dir=<DIR>      - undocumented
#       --skip=<DIR>     - undocumented
#       --start-at=<DIR> - undocumented
parse_cmdline () {
  _suite="$1"
  shift

  # This is the only place where test dirs are specified.
  # Do not duplicate this line
  dirs="*[0-9]*"

  continueOnError="n"

  for opt; do
    case "$opt" in
      -k | --keep-going)
        continueOnError="y"
        ;;
      -j*)
        NPROC=${opt#-j}
        ;;
      --dir=*)
        dirs="$(echo "$opt" | sed -e 's/--dir=//')"
        ;;
      --skip=*)
        d="$(echo "$opt" | sed -e 's/--skip=//')"
        dirs="$(echo "" $dirs | sed -e "s/ $d//")"
        ;;
      --start-at=*)
        d="$(echo "$opt" | sed -e 's/--start-at=//')"
        dirs="$(echo "" $dirs | sed -e "s/^.* $d//")"
        dirs="$d $dirs"
        ;;
      --list-tests)
        echo $dirs
        exit 0
        ;;
      *)
        printf "Unknown option %s\n" "$opt"
        exit 2
        ;;
    esac
  done

  # If option '-j' was not used, set NPROC to number of available CPUs
  NPROC=${NPROC:-$(nproc 2> /dev/null || sysctl -n hw.ncpu 2> /dev/null || echo 1)}
}

# Run a single testcase by starting a testcase-local testsuite.sh and writing all outputs to a 'test.log' file.
# The working directory is moved into the testcase's directory.
# Failing tests are appended to a '*.failures' file
# Partial JUnit XML results are saved in the testsuite's directory. One file per testcase.
#
# Global variables:
# * $_suite - testsuite name (parent directory name)
# * $ANSI_RED
# * $ANSI_GREEN
# * $ANSI_NOCOLOR
#
# Parameters:
# $1 - directory/name of the testcase
# $2 - if set to 'n' don't continue: print error message and exit
singlerun() {
  local testName="$1"
  local continueOnError="$2"

  # A missing directory has to be caught here. Falling through to the 'cd' below would leave the working
  # directory on the suite, where './testsuite.sh' is the suite's own launcher - the whole suite would run
  # again and be reported as one passing testcase.
  if [ ! -d "${testName}" ]; then
    printf "%s %s: ${ANSI_RED}no such testcase${ANSI_NOCOLOR}\n" "${_suite}" "${testName}"
    printf '%s ' "${testName}" >> "${_suite}.failures"
    printf '    <testcase classname="%s" name="%s" time="0.000">\n      <error message="no such testcase" type="error" />\n    </testcase>\n' \
      "${_suite}" "${testName}" > "${testName}.testresult"
    return 0
  fi

  ( # Use a subshell, so old working directory can be properly restored. pushd/popd are too noisy.
    cd "${testName}"
    local startTime=$(now_nanoseconds)

    printf -- "------------------------\n"
    # 'set -e' is active, so the exit code has to be read from a condition. Written as a plain assignment,
    # a failing testcase would end this script before the failure could be recorded.
    if ./testsuite.sh > test.log 2>&1; then
      exitCode=0
    else
      exitCode=$?
    fi
    printf -- "========================\n"

    local stopTime=$(now_nanoseconds)
    local elapsedTime=$(elapsed_seconds ${startTime} ${stopTime})

    if [ $exitCode -eq 0 ]; then
      printf "%s %s: ${ANSI_GREEN}ok${ANSI_NOCOLOR}\n" "${_suite}" "${testName}"

      # Write JUnit testcase success result into a partial XML file
      printf '    <testcase classname="%s" name="%s" time="%s" />\n' \
        "${_suite}" "${testName}" "${elapsedTime}"  > "../${testName}.testresult"
      # Don't display log
    else
      printf "%s %s: ${ANSI_RED}failed${ANSI_NOCOLOR}\n" "${_suite}" "${testName}"
      printf '%s ' "${testName}" >> "../${_suite}.failures"

      # Write JUnit testcase failure result into a partial XML file and embed the log as the failure message
      printf '    <testcase classname="%s" name="%s" time="%s">\n      <failure message="testcase failed" type="failure">' \
        "${_suite}" "${testName}" "${elapsedTime}"  > "../${testName}.testresult"
      xml_escape test.log                          >> "../${testName}.testresult"
      printf '</failure>\n    </testcase>\n'        >> "../${testName}.testresult"

      # If continueOnError is 'n', print test.log to console and stop the run. The subshell's non-zero exit
      # reaches the caller through 'set -e'.
      if [ "$continueOnError" = "n" ]; then
        cat test.log
        exit 1
      fi
    fi

    # A recorded failure must not end the subshell on a false condition, or 'set -e' would stop the run even
    # when the caller asked to keep going.
    exit 0
  )
}

# Run all testcases if possible in parallel.
#
# Global variables:
# * $_suite       - testsuite name (parent directory name)
# * $NPROC        - number of parallel instances (jobs)
# * $dirs         - list of testcases (directories)
# * $ANSI_RED     - ANSI color escape code for red text
# * $ANSI_GREEN   - ANSI color escape code for green text
# * $ANSI_NOCOLOR - ANSI color escape code to reset text color
allrun () {
  # Remove all partial XML files
  rm -f *.testresult
  # Reset list of failed testcases by overwriting '*.failures' file's content
  printf '' > ${_suite}.failures

  # The list of testcases has to be counted before the branch, as both paths report it.
  local testCount=$(printf '%s ' ${dirs} | wc -w)

  # If xargs program exists and NPROC > 1, run tests in parallel using multiple jobs (script instances)
  if command -v xargs >/dev/null 2>&1 && [ "${NPROC}" -gt 1 ]; then
    printf "Running with %s test workers ...\n" ${NPROC} >&2

    local batchSize=$((1 + testCount / NPROC))
    local batchSize=$(( batchSize > 10 ? 10 : batchSize ))
    # The workers source this file, which is bash. Running them under 'sh' leaves ANSI escapes, 'local' and
    # the arithmetic below to whatever /bin/sh happens to be.
    echo ${dirs} | DO_ALLRUN=0 xargs -P${NPROC} -n${batchSize} bash -c \
      's=$1; _suite=$2 continueOnError=$3; shift 3; . "$s";
       for i in "$@"; do singlerun "$i" "$continueOnError" || true; done' \
      \
      bash "$0" "${_suite}" "$continueOnError" || true
  else
    for i in ${dirs}; do
      singlerun "$i" "$continueOnError"
    done
  fi

  if [ ! -f ${_suite}.failures ]; then
    printf "error: Couldn't find test driver generated '%s'!\n" "${_suite}.failures" >&2
    exit 1
  fi

  local failureCount="$(cat "${_suite}.failures" | wc -w)"
  if [ $failureCount -eq 0 ]; then
    printf "%s: %s tests are ${ANSI_GREEN}successful${ANSI_NOCOLOR}\n" "${_suite}" "$testCount" && exit 0
  else
    local failures="$(cat "${_suite}.failures")"
    for failed in $failures; do
      printf "%s %s: ${ANSI_RED}failed${ANSI_NOCOLOR}\n" "${_suite}" "$failed"
      cat "$failed/test.log"
      printf '\n\n'
    done

    printf "%s: %s out of %s tests ${ANSI_RED}failed${ANSI_NOCOLOR} (%s)\n" "${_suite}" "$failureCount" "$testCount" "$failures" && exit 1
  fi
}

if [ "$DO_ALLRUN" != 0 ]; then
  parse_cmdline "$@"
  allrun
fi
