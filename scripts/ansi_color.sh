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
#
# ==============================================================================
#  Copyright (C) 2017-2025 Patrick Lehmann - Boetzingen, Germany
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
