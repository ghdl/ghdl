#! /usr/bin/env bash

# Stop in case of error
set -e

# The colours, the log sections and the report helpers all come from the toolbox.
. "$(dirname "${BASH_SOURCE[0]}")/../scripts/bash_toolbox.sh"

# Display an error message in red and exit.
# In case multiple arguments are given, display multiple error messages line-by-line.
# $1 - error message
die() {
  printf -- "${ANSI_RED}%s${ANSI_NOCOLOR}\n" "$@" >&2
  exit 1
}

# The VESTS testsuite: compliance testsuite, from: https://github.com/nickg/vests.git 388250486a
_vests () {
  ( # Use a subshell, so old working directory can be properly restored. pushd/popd are too noisy.
    cd vests

    startTime=$(now_nanoseconds)

    # 'set -e' is active, so the exit code has to be read from a condition.
    if ./testsuite.sh > vests.log 2>&1; then
      exitCode=0
    else
      exitCode=$?
    fi

    stopTime=$(now_nanoseconds)
    elapsedTime=$(elapsed_seconds ${startTime} ${stopTime})

    # The result is written where the merge step looks for it: one '*.testresult' file inside the suite's
    # directory, the same convention suite_driver.sh follows.
    if [[ $exitCode -eq 0 ]]; then
      printf -- "${ANSI_GREEN}Vests is OK$ANSI_NOCOLOR\n"
      wc -l vests.log

      printf -- '    <testcase classname="%s" name="%s" time="%s" />\n' \
        "vests" "all" "$elapsedTime" > "all.testresult"
    else
      cat vests.log
      printf -- "${ANSI_RED}Vests failure$ANSI_NOCOLOR\n"

      printf -- '    <testcase classname="%s" name="%s" time="%s">\n      <failure message="vests failed" type="failure">' \
        "vests" "all" "$elapsedTime" > "all.testresult"
      xml_escape vests.log          >> "all.testresult"
      printf -- '</failure>\n    </testcase>\n' >> "all.testresult"
    fi

    # The exit code has to leave the subshell, or a vests failure is invisible to the caller.
    exit $exitCode
  )
}

#---

if [[ -z "$GHDL" ]]; then
  if [[ -n "$prefix" ]]; then
    export GHDL="$prefix/bin/ghdl"
  elif [[ -n "$(command -v which)" ]]; then
    export GHDL="$(which ghdl)"
  else
    die "error: GHDL environment variable is not defined"
  fi
fi

if [[ -z "$GHWDUMP" ]]; then
  case "$GHDL" in
    */*)
      export GHWDUMP=${GHDL%/*}/ghwdump
      ;;
    *)
      export GHWDUMP=ghwdump
      ;;
  esac
fi

command -v "$GHWDUMP" >/dev/null || die "ghwdump executable not found: $GHWDUMP"

# The toolbox prints nothing when it is sourced, so the warning about a coarse clock is issued here, once.
if [[ ${HAS_NANOSECONDS} -eq 0 ]]; then
  printf -- "${ANSI_YELLOW}WARNING:${ANSI_NOCOLOR} %s has no nanosecond format, durations are rounded to whole seconds.\n" "${DATE}" >&2
  printf -- "         Install GNU coreutils for real durations: brew install coreutils\n" >&2
fi
command -v "diff"     >/dev/null || die "diff executable not found"

# Set working directory to directory of this script
cd $(dirname "$0")
# Remove result files from previous runs, so a stale report is never mistaken for this run's. The per-testcase
# snippets live in the suite directories.
rm -f *.testresults *.testresults.xml testsuites.xml
rm -f */*.testresult */*.failures
rm -f test_ok

failures=""
testsuites=

for opt; do
  shift
  case "$opt" in
    [a-z]*)
      testsuites="$testsuites $opt"
      ;;
    --)
      break
      ;;
    *)
      printf -- "%s: unknown option '%s'\n" "$0" "$opt"
      exit 2
      ;;
  esac
done

if [[ -z "$testsuites" ]]; then
  testsuites="ghdlversion ghdlhelp sanity pyunit gna vests synth vpi vhpi"
fi

printf -- "> tests:%s\n" "$testsuites"
printf -- "> args: %s\n" "$@"

# Run a testsuite
run_testsuite() {
  case $1 in
    help)
      printf -- "Usage:\n"
      printf -- "  ./testsuite.sh                     run all testsuites\n"
      printf -- "  ./testsuite.sh <suite>             run single testsuite\n"
      printf -- "  ./testsuite.sh <suite> <suite> ... run multiple testsuites\n"
      printf -- "  ./testsuite.sh <suite> -- <option> options after -- are passed to the suite\n"
      printf -- "\n"
      printf -- "Options:\n"
      printf -- "  -j<N>                              run testcases using <N> parallel jobs\n"
      printf -- "  -k  --keep-going                   continue after errors\n"
      printf -- "\n"
      printf -- "Supported testsuites:\n"
      printf -- " * sanity\n"
      printf -- " * gna\n"
      printf -- " * synth\n"
      printf -- " * vpi\n"
      printf -- " * vhpi\n"
      printf -- " * vests\n"
      printf -- " * pyunit\n"
      printf -- "\n"
      exit
      ;;
    sanity|gna|synth|vpi|vhpi)
      section_start "[GHDL - test] $1"
      ( # Use a subshell, so old working directory can be properly restored. pushd/popd are too noisy.
        cd "$1"
        ../suite_driver.sh "$@"
      )
      local exitCode=$?
      section_end
      # The caller decides what a failing suite means. Exiting here would skip the report, which is exactly
      # what a failing run needs.
      return $exitCode
      ;;
    pyunit)
      section_start "[GHDL - test] pyunit"
      PYTHONPATH=$(pwd)/.. ${PYTHON:-python3} -m pytest -vsrA pyunit
      section_end
      ;;
    vests)
      section_start "[GHDL - test] vests"
      _vests
      local exitCode=$?
      section_end
      return $exitCode
      ;;
    ghdlversion)
      section_start "GHDL is: $GHDL"
      $GHDL version
      printf -- "REF:  %s\n" "$($GHDL version ref)"
      printf -- "HASH: %s\n" "$($GHDL version hash)"
      section_end
      ;;
    ghdlhelp)
      section_start "GHDL help"
      $GHDL help
      section_end
      ;;
    *)
      die "$0: test name '$1' is unknown"
      ;;
  esac
}

globalTimestamp="$(now_timestamp)"
totalStartTime=$(now_nanoseconds)
totalTestCount=0
totalFailedCount=0
totalErroredCount=0
totalSkippedCount=0
overallExitCode=0
hostName="$(hostname 2> /dev/null || echo "")"

# Run testsuites individually in a sequence.
# Each testsuite might run testcases in parallel.
reportedTestsuites=""
for testsuite in $testsuites; do
  timestamp="$(now_timestamp)"
  startTime=$(now_nanoseconds)

  # Run a testsuite. A failing suite must not end this script: the report is written below, and a run that
  # failed is the one whose report is worth having.
  set +e
  run_testsuite "${testsuite}" "$@"
  suiteExitCode=$?
  set -e

  if [[ ${suiteExitCode} -ne 0 ]]; then
    overallExitCode=1
  fi

  stopTime=$(now_nanoseconds)
  elapsedTime=$(elapsed_seconds ${startTime} ${stopTime})

  # Not every testsuite reports testcases. 'pyunit' writes its own report, and 'ghdlversion' and 'ghdlhelp'
  # are checks without testcases, so they contribute nothing to merge.
  if ! ls ${testsuite}/*.testresult > /dev/null 2>&1; then
    continue
  fi
  reportedTestsuites="${reportedTestsuites} ${testsuite}"

  # Merge testcases
  cat ${testsuite}/*.testresult > "${testsuite}.testresults"

  # Extract statistics from *.testresults, so testsuite can be assembled
  testCount=$(   count_elements "${testsuite}.testresults" "testcase")
  failedCount=$( count_elements "${testsuite}.testresults" "failure")
  erroredCount=$(count_elements "${testsuite}.testresults" "error")
  skippedCount=$(count_elements "${testsuite}.testresults" "skipped")
  # Accumulate statistics
  totalTestCount=$((   totalTestCount    + testCount))
  totalFailedCount=$(( totalFailedCount  + failedCount))
  totalErroredCount=$((totalErroredCount + erroredCount))
  totalSkippedCount=$((totalSkippedCount + skippedCount))

  # Create a partial XML file for every testsuite
  printf -- '  <testsuite name="%s" tests="%s" failures="%s" errors="%s" skipped="%s" time="%s" timestamp="%s" hostname="%s">\n' \
    "${testsuite}" "${testCount}" "${failedCount}" "${erroredCount}" "${skippedCount}" "${elapsedTime}" "${timestamp}" "${hostName}" \
                                  > "${testsuite}.testresults.xml"
  cat "${testsuite}.testresults" >> "${testsuite}.testresults.xml"
  printf -- '  </testsuite>\n'      >> "${testsuite}.testresults.xml"

  # The snippets have been merged, so they are of no further use. A full run writes one per testcase - over a
  # thousand of them - and leaving them behind means the next run's glob and any artifact upload pick them up.
  rm -f ${testsuite}/*.testresult "${testsuite}/${testsuite}.failures" "${testsuite}.testresults"
done

totalStopTime=$(now_nanoseconds)
totalElapsedTime=$(elapsed_seconds ${totalStartTime} ${totalStopTime})

# Create final testsuites XML file
section_start "Merge testreports"
printf -- '<?xml version="1.0" encoding="utf-8"?>
<testsuites name="ghdl" tests="%s" failures="%s" errors="%s" skipped="%s" time="%s" timestamp="%s">\n' \
  "${totalTestCount}" "${totalFailedCount}" "${totalErroredCount}" "${totalSkippedCount}" "${totalElapsedTime}" "${globalTimestamp}" \
                                     >  "testsuites.xml"
for testsuite in ${reportedTestsuites}; do
  cat "${testsuite}.testresults.xml" >> "testsuites.xml"
done
printf -- "</testsuites>\n"   >> "testsuites.xml"
printf -- "Wrote %s: %s tests, %s failed, %s errored, %s skipped.\n" \
  "testsuites.xml" "${totalTestCount}" "${totalFailedCount}" "${totalErroredCount}" "${totalSkippedCount}"
section_end
if [[ ${overallExitCode} -ne 0 ]]; then
  printf -- "${ANSI_RED}[GHDL - test] FAILED${ANSI_NOCOLOR}\n"
  exit 1
fi

printf -- "${ANSI_GREEN}[GHDL - test] SUCCESSFUL${ANSI_NOCOLOR}\n"
touch test_ok
