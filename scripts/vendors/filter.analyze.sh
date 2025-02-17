#! /usr/bin/env bash
# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
#  Bash Script:  STDOUT Post-Processor for GHDL analyze (-a)
#
# Description:
# ------------------------------------
#  This is a Bash script (executable) which:
#    - creates a subdirectory in the current working directory
#    - compiles all OSVVM packages
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

# work around for Darwin (Mac OS)
test greadlink --version > /dev/null 2>&1 && READLINK=greadlink || READLINK=readlink

# Save working directory
WorkingDir=$(pwd)
ScriptDir="$($READLINK -f $(dirname $0))"
RootDir="$($READLINK -f "$ScriptDir"/..)"

source "$ScriptDir"/../ansi_color.sh
if [[ $? -ne 0 ]]; then
	printf "\x1b[31m[ERROR] %s\x1b[0m\n" "While loading Bash utilities." 1>&2
	exit 1
fi

# command line argument processing
COMMAND=2
INDENT=""
VERBOSE=0
DEBUG=0
while [ "$#" -gt 0 ]; do
	key="$1"
	case $key in
		-i|--indent)
		shift
		INDENT=$1
		;;
		-v|--verbose)
		VERBOSE=1
		;;
		-d|--debug)
		VERBOSE=1
		DEBUG=1
		;;
		-h|--help)
		COMMAND=0
		;;
		*)		# unknown option
		PrintError "Unknown command line option '$key'."
		COMMAND=1
		;;
	esac
	shift # past argument or value
done

if [ $COMMAND -le 1 ]; then
	printf "\n"
	printf "%s\n"   "Synopsis:"
	printf "  %s\n"   "Script to filter GHDL analyze (-a) outputs."
	printf "\n"
	printf "%s\n"   "Usage:"
	printf "  %s\n"   "filter.analyze.sh [-v][-d] [--help] [--indent <pattern>]"
	printf "\n"
	printf "%s\n" "Common commands:"
	printf "  %-2s %-20s %s\n" "-h" "--help"             "Print this help page."
	printf "\n"
	printf "%s\n" "Common options:"
	printf "  %-2s %-20s %s\n" "-v" "--verbose"          "Print verbose messages."
	printf "  %-2s %-20s %s\n" "-d" "--debug"            "Print debug messages."
	printf "  %-2s %-20s %s\n" "-i" "--indent <pattern>" "Indent all lines by this pattern."
	printf "\n"
	exit $COMMAND
fi


# States
ST_Common=1
ST_Exception=2
ST_FurtherLine_1=3
ST_FurtherLine_2=4

# Current state
State=0

# Counters
Counter_Warning=0
Counter_Error=0
Counter_Fatal=0

while IFS= read -r line; do
	line="${line//\\//}"
	case $State in
		0|$ST_Common)
			if [[ $line = *"ghdl1-llvm"* ]]; then
				printf "%s\n" "$INDENT${ANSI_DARK_GRAY}$line${ANSI_NOCOLOR}"
			elif [[ $line = *":"*":"*":warning: "* ]]; then
				State=$ST_FurtherLine_1

				let Counter_Warning++
				printf "%s\n" "$INDENT${ANSI_YELLOW}WARNING:${ANSI_NOCOLOR} $line"
			elif [[ $line = *":"*":"*":note: "* ]]; then
				printf "%s\n" "$INDENT${ANSI_CYAN}NOTE:${ANSI_NOCOLOR} $line"
			elif [[ $line = *":"*":"*": "* ]]; then
				State=$ST_FurtherLine_1

				let Counter_Error++
				printf "%s\n" "$INDENT${ANSI_RED}ERROR:${ANSI_NOCOLOR} $line"
			elif [[ $line = *"compilation error"* ]]; then
				printf "%s\n" "$INDENT${ANSI_RED}FATAL:${ANSI_NOCOLOR} $line"
			elif [[ ${line:0:66} == "******************** GHDL Bug occurred ***************************" ]]; then
				State=$ST_Exception
				Counter_Fatal=1
				printf "%s\n" "$INDENT${ANSI_MAGENTA}$line${ANSI_NOCOLOR}"
			else
				if [[ $(printf '%d\n' "'$line") == "13" ]]; then
					echo ""
				else
					printf "%s\n" "$INDENT${ANSI_RED}SCRIPT ERROR: Unfiltered line${ANSI_NOCOLOR}"
					printf "%s\n" "$INDENT$line"
				fi
			fi
		;;
		$ST_Exception)
			printf "%s\n" "$INDENT${ANSI_MAGENTA}$line${ANSI_NOCOLOR}"
			if [[ ${line:0:66} == "******************************************************************" ]]; then
				State=$ST_Common
			fi
		;;
		$ST_FurtherLine_1)
			State=$ST_FurtherLine_2
			printf "%s\n" "$INDENT$line"
		;;
		$ST_FurtherLine_2)
			State=$ST_Common
			printf "%s\n" "$INDENT$line"
		;;
	esac
done < "/dev/stdin"

exit $(( ( $Counter_Fatal != 0 ? $Counter_Fatal*4+3 : ( $Counter_Error != 0 ? $Counter_Error*4+2 : ( $Counter_Warning != 0 ? $Counter_Warning*4+1 : 0 ))) ))
