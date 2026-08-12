#! /usr/bin/env bash
# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
#  Bash procedure: Collection of useful Bash procedures
#
# Description:
# ------------------------------------
#  - Color codes
#  - Printing and text formatting
#  - Error handling
#  - Log sections, grouped when running in a CI environment
#  - Time measurement and timestamps
#  - XML escaping, for writing test reports
#
# ==============================================================================
#  Copyright (C) 2017-2026 Patrick Lehmann - Boetzingen, Germany
#  Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
# ==============================================================================
VERBOSE=${VERBOSE:-0}
DEBUG=${DEBUG:-0}

enable_color() {
	ANSI_ESC=$'\x1b'
	ANSI_BLACK=$'\x1b[30m'
	ANSI_RED=$'\x1b[31m'
	ANSI_GREEN=$'\x1b[32m'
	ANSI_YELLOW=$'\x1b[33m'
	ANSI_BLUE=$'\x1b[34m'
	ANSI_MAGENTA=$'\x1b[35m'
	ANSI_CYAN=$'\x1b[36m'
	ANSI_DARK_GRAY=$'\x1b[90m'
	ANSI_LIGHT_GRAY=$'\x1b[37m'
	ANSI_LIGHT_RED=$'\x1b[91m'
	ANSI_LIGHT_GREEN=$'\x1b[92m'
	ANSI_LIGHT_YELLOW=$'\x1b[93m'
	ANSI_LIGHT_BLUE=$'\x1b[94m'
	ANSI_LIGHT_MAGENTA=$'\x1b[95m'
	ANSI_LIGHT_CYAN=$'\x1b[96m'
	ANSI_WHITE=$'\x1b[97m'
	ANSI_NOCOLOR=$'\x1b[0m'

	# red texts
	COLORED_ERROR="${ANSI_RED}[ERROR]"
	COLORED_WARNING="${ANSI_YELLOW}[WARNING]"
	COLORED_FAILED="${ANSI_RED}[FAILED]${ANSI_NOCOLOR}"

	# green texts
	COLORED_DONE="${ANSI_GREEN}[DONE]${ANSI_NOCOLOR}"
	COLORED_SUCCESSFUL="${ANSI_GREEN}[SUCCESSFUL]${ANSI_NOCOLOR}"
}

disable_color() {
	unset ANSI_BLACK ANSI_RED ANSI_GREEN ANSI_YELLOW ANSI_BLUE ANSI_MAGENTA ANSI_CYAN ANSI_DARK_GRAY
	unset ANSI_LIGHT_GRAY ANSI_LIGHT_RED ANSI_LIGHT_GREEN ANSI_LIGHT_YELLOW ANSI_LIGHT_BLUE ANSI_LIGHT_MAGENTA ANSI_LIGHT_CYAN
	unset ANSI_NOCOLOR

	COLORED_ERROR="[ERROR]"
	COLORED_WARNING="[WARNING]"
	COLORED_FAILED="[FAILED]"

	COLORED_DONE="[DONE]"
	COLORED_SUCCESSFUL="[SUCCESSFUL]"
}

enable_color

test $VERBOSE -eq 1 && printf "  ${ANSI_DARK_GRAY}%s${ANSI_NOCOLOR}\n" "Declaring common Bash procedures ..."
test $DEBUG -eq 1 &&   printf "    ${ANSI_DARK_GRAY}%s${ANSI_NOCOLOR}\n" "procedure DeclareProcedure( <procName> <parameters> )"
DeclareProcedure() {
	if [[ $DEBUG -eq 1 ]]; then
		printf "    ${ANSI_DARK_GRAY}procedure %s( %s )${ANSI_NOCOLOR}\n" "${@: -2:1}" "${@: -1:1}"
	fi
}

DeclareProcedure "Chapter" "<title>"
Chapter() {
	case $# in
		1) local indent="";   local color="${ANSI_MAGENTA}"; local message="$1" ;;
		2) local indent="$1"; local color="${ANSI_MAGENTA}"; local message="$2" ;;
		3) local indent="$1"; local color="$2";              local message="$3" ;;
	esac
	printf "${indent}${color}%s${ANSI_NOCOLOR}\n" "${message}"
}

DeclareProcedure "Section" "<title>"
Section() {
	case $# in
		1) local indent="";   local color="${ANSI_LIGHT_CYAN}"; local message="$1" ;;
		2) local indent="$1"; local color="${ANSI_LIGHT_CYAN}"; local message="$2" ;;
		3) local indent="$1"; local color="$2";                 local message="$3" ;;
	esac
	printf "${indent}${color}%s${ANSI_NOCOLOR}\n" "${message}"
}

DeclareProcedure "SubSection" "<title>"
SubSection() {
	case $# in
		1) local indent="";   local color="${ANSI_YELLOW}"; local message="$1" ;;
		2) local indent="$1"; local color="${ANSI_YELLOW}"; local message="$2" ;;
		3) local indent="$1"; local color="$2";             local message="$3" ;;
	esac
	printf "${indent}${color}%s${ANSI_NOCOLOR}\n" "${message}"
}

DeclareProcedure "PrintNormal" "[<indent> [<color>]] <message>"
PrintNormal() {
	case $# in
		1) local indent="";   local color="${ANSI_LIGHT_CYAN}"; local message="$1" ;;
		2) local indent="$1"; local color="${ANSI_LIGHT_CYAN}"; local message="$2" ;;
		3) local indent="$1"; local color="$2";                 local message="$3" ;;
	esac
	printf "${indent}${color}%s${ANSI_NOCOLOR}\n" "${message}"
}

DeclareProcedure "PrintVerbose" "[<indent> [<color>]] <message>"
PrintVerbose() {
	if [[ $VERBOSE -eq 1 ]]; then
		case $# in
			1) local indent="  "; local color="${ANSI_CYAN}"; local message="$1" ;;
			2) local indent="$1"; local color="${ANSI_CYAN}"; local message="$2" ;;
			3) local indent="$1"; local color="$2";           local message="$3" ;;
		esac
		printf "${indent}${color}%s${ANSI_NOCOLOR}\n" "${message}"
	fi
}

DeclareProcedure "PrintDebug" "[<indent> [<color>]] <message>"
PrintDebug() {
	if [[ $DEBUG -eq 1 ]]; then
		case $# in
			1) local indent="    "; local color="${ANSI_DARK_GRAY}"; local message="$1" ;;
			2) local indent="$1";   local color="${ANSI_DARK_GRAY}"; local message="$2" ;;
			3) local indent="$1";   local color="$2";                local message="$3" ;;
		esac
		printf "${indent}${color}%s${ANSI_NOCOLOR}\n" "${message}"
	fi
}

DeclareProcedure "PrintWarning" "[<indent>] <message>"
PrintWarning() {
	case $# in
		1) local indent="";   local message="$1" ;;
		2) local indent="$1"; local message="$2" ;;
	esac
	printf "${indent}${COLORED_WARNING} %s${ANSI_NOCOLOR}\n" "${message}"
}

DeclareProcedure "PrintError" "[<indent>] <message>"
PrintError() {
	case $# in
		1) local indent="";   local message="$1" ;;
		2) local indent="$1"; local message="$2" ;;
	esac
	printf "${indent}${COLORED_ERROR} %s${ANSI_NOCOLOR}\n" "${message}" 1>&2
}

DeclareProcedure "PrintErrorAndExit" "<message> <exitCode=1>"
PrintErrorAndExit() {
	PrintError "$1"

	local exitCode=${2:-1}
	if [[ $exitCode -gt 0 ]]; then
		exit $exitCode
	fi
}

DeclareProcedure "ContinueErrorAndExit" "<message> <exitCode=1>"
ContinueErrorAndExit() {
	printf "  ${ANSI_LIGHT_RED}%s${ANSI_NOCOLOR}\n" "$1" 1>&2

	local exitCode=${2:-1}
	if [[ $exitCode -gt 0 ]]; then
		exit $exitCode
	fi
}

DeclareProcedure "CheckError" "<returnCode> <message> <exitCode=1>"
CheckError() {
	local returnCode=$1
	local message="$2"
	local exitCode=${3:-1}

	if [[ $returnCode -ne 0 ]]; then
		PrintError "$2"

		if [[ $exitCode -gt 0 ]]; then
			exit $exitCode
		fi
	fi
}

DeclareProcedure "CheckErrorOrContinue" "<returnCode> <continueOnerror> <indent> <message> <exitCode=1>"
CheckErrorOrContinue() {
	local returnCode=$1
	local continueOnError=$2
	local indent="$3"
	local message="$4"
	local exitCode=${5:-1}

	if [[ $returnCode -ne 0 ]]; then
		PrintError "$3" "$4 ExitCode: $returnCode"
		test $continueOnError -eq 0 && exit $exitCode
	fi
}

# ==============================================================================
# Log sections
# ==============================================================================
# A section groups the output of one step. In a CI environment the group is
# collapsible and its duration is reported; elsewhere it is a coloured heading.
#
# The two variants are defined once, here, rather than being overridden by every
# script that needs them.
if [[ -n "${GITHUB_ACTIONS}" ]]; then
	DeclareProcedure "section_start" "<title> [<color>]"
	section_start() {
		local color="${2:-${ANSI_YELLOW}}"

		printf -- '::group::'
		printf -- "${color}%s${ANSI_NOCOLOR}\n" "$1"
		SECONDS=0
	}

	DeclareProcedure "section_end" ""
	section_end() {
		local duration=$SECONDS

		printf -- '::endgroup::\n'
		printf -- "${ANSI_DARK_GRAY}Took %d min %d sec.${ANSI_NOCOLOR}\n" "$((duration / 60))" "$((duration % 60))"
	}
else
	DeclareProcedure "section_start" "<title> [<color>]"
	section_start() {
		local color="${2:-${ANSI_YELLOW}}"

		printf -- "${color}%s${ANSI_NOCOLOR}\n" "$1"
	}

	DeclareProcedure "section_end" ""
	section_end() {
		:
	}
fi


# ==============================================================================
# Time measurement and timestamps
# ==============================================================================
# The date program to use.
#
# A nanosecond clock ('%N') and an ISO 8601 offset ('%:z') are needed, and BSD date has neither. On macOS,
# install GNU coreutils - 'brew install coreutils' - which provides 'gdate'. The CI workflow additionally puts
# the coreutils 'gnubin' directory on PATH, so that plain 'date' is the GNU one there.
if command -v gdate > /dev/null 2>&1; then
	DATE="gdate"
else
	DATE="date"
fi

# Whether that date program understands the two formats. Probed once, as probing per testcase would cost more
# than the measurement is worth. A caller that wants to warn about it reads HAS_NANOSECONDS; this file prints
# nothing, so that sourcing it can never contaminate a captured value.
if [[ "$(${DATE} +%N)" == "N" ]]; then
	HAS_NANOSECONDS=0
else
	HAS_NANOSECONDS=1
fi

case "$(${DATE} +%:z)" in
	[+-][0-9][0-9]:[0-9][0-9]) TIMEZONE_FORMAT="%:z" ;;
	*)                         TIMEZONE_FORMAT="%z"  ;;
esac

DeclareProcedure "now_nanoseconds" ""
# Read the clock, in nanoseconds.
# Without '%N' the reading is a whole second, which keeps the arithmetic working rather than producing a
# literal 'N' that would abort the caller.
now_nanoseconds() {
	if [[ ${HAS_NANOSECONDS} -eq 1 ]]; then
		${DATE} +%s%N
	else
		printf -- '%s000000000' "$(${DATE} +%s)"
	fi
}

DeclareProcedure "elapsed_seconds" "<startTime> <stopTime>"
# Print a duration in seconds, from a start and a stop reading of now_nanoseconds.
# The fraction is zero-padded: 8 ms is '0.008', not '0.8'.
elapsed_seconds() {
	local milliseconds=$((($2 - $1) / 1000000))

	printf -- '%d.%03d' $((milliseconds / 1000)) $((milliseconds % 1000))
}

DeclareProcedure "now_timestamp" ""
# Print the current time as an ISO 8601 timestamp, as the 'timestamp' attribute of a JUnit report takes.
now_timestamp() {
	${DATE} +"%Y-%m-%dT%H:%M:%S${TIMEZONE_FORMAT}"
}


# ==============================================================================
# XML
# ==============================================================================
# An optional prefix for the names written into a test report.
#
# Merging the reports of several platforms collapses them when the names collide: 'sanity/000hello' from Ubuntu,
# Windows and macOS are one testcase as far as a merge tool can tell. Setting GHDL_TEST_VARIANT to something
# unique per platform - 'ubuntu-26.04-mcode' - keeps them apart.
REPORT_PREFIX="${GHDL_TEST_VARIANT:+${GHDL_TEST_VARIANT}.}"


DeclareProcedure "xml_escape" "<file>"
# Read a file and write it as XML character data.
#
# ANSI colour sequences are removed, the characters XML gives a meaning to are escaped, and the control
# characters XML 1.0 forbids outright - the escape character among them - are dropped. Without this, a single
# failing testcase makes a whole report unparsable, because a test log holds both markup characters and the
# colour sequences GHDL writes.
#
# A log is not necessarily UTF-8 either. The VHDL sources under 'vests' are Latin-1, and GHDL echoes the
# offending byte when it rejects a character, so a lone 0xAB reaches the log and would make the report invalid
# against its own encoding declaration. Bytes that are not valid UTF-8 are dropped.
xml_escape() {
	local reencode="cat"
	if command -v iconv > /dev/null 2>&1; then
		reencode="iconv -f UTF-8 -t UTF-8 -c"
	fi

	sed -e "s/${ANSI_ESC}\[[0-9;?]*[a-zA-Z]//g" \
	    -e 's/&/\&amp;/g' \
	    -e 's/</\&lt;/g' \
	    -e 's/>/\&gt;/g' \
	    -e 's/"/\&quot;/g' \
	    -e "s/'/\&apos;/g" \
	    "$1" | tr -d '\000-\010\013\014\016-\037' | ${reencode}
}

DeclareProcedure "count_elements" "<file> <elementName>"
# Count the occurrences of an XML element in a file, as assembling a report needs the numbers for its attributes.
count_elements() {
	grep -c -- "<$2" "$1" 2> /dev/null || true
}
