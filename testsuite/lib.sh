#! /usr/bin/env bash

# Helpers shared by testsuite.sh and suite_driver.sh.
#
# This file is only sourced, never executed. Both scripts run with the testsuite directory either as their own
# directory or as the parent of it, so both reach this file by a fixed relative path.

# The escape character, built without a shell-specific escape so this file can be sourced by any shell.
ESC=$(printf '\033')

# Print a duration in seconds, from a start and a stop reading of a nanosecond clock.
# The fraction is zero-padded: 8 ms is '0.008', not '0.8'.
#
# Parameters:
# $1 - start time in nanoseconds
# $2 - stop time in nanoseconds
elapsed_seconds () {
  local milliseconds=$((($2 - $1) / 1000000))
  printf '%d.%03d' $((milliseconds / 1000)) $((milliseconds % 1000))
}

# Read a log file and write it as XML character data.
#
# ANSI colour sequences are removed, the characters XML gives a meaning to are escaped, and the control
# characters XML 1.0 forbids outright - the escape character among them - are dropped. Without this, a single
# failing testcase makes the whole report unparsable, because a test log holds both markup characters and the
# colour sequences GHDL writes.
#
# A test log is not necessarily UTF-8 either. The VHDL sources under 'vests' are Latin-1, and GHDL echoes the
# offending byte when it rejects a character, so a lone 0xAB reaches the log and would make the report invalid
# against its own encoding declaration. Bytes that are not valid UTF-8 are dropped.
#
# Parameters:
# $1 - the file to convert
xml_escape () {
  local reencode="cat"
  if command -v iconv > /dev/null 2>&1; then
    reencode="iconv -f UTF-8 -t UTF-8 -c"
  fi

  sed -e "s/${ESC}\[[0-9;?]*[a-zA-Z]//g" \
      -e 's/&/\&amp;/g' \
      -e 's/</\&lt;/g' \
      -e 's/>/\&gt;/g' \
      -e 's/"/\&quot;/g' \
      -e "s/'/\&apos;/g" \
      "$1" | tr -d '\000-\010\013\014\016-\037' | ${reencode}
}

# Count the occurrences of an XML element in a file, as the merge step needs the numbers for the attributes.
#
# Parameters:
# $1 - the file to search
# $2 - the element name, without the angle bracket
count_elements () {
  grep -c -- "<$2" "$1" 2> /dev/null || true
}
