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
#  Copyright (C) 2017-2021 Patrick Lehmann - Boetzingen, Germany
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
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading Bash utilities.${ANSI_NOCOLOR}"    ; exit 1; fi

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
		echo 1>&2 -e "${COLORED_ERROR} Unknown command line option '$key'.${ANSI_NOCOLOR}"
		COMMAND=1
		;;
	esac
	shift # past argument or value
done

if [ $COMMAND -le 1 ]; then
	echo ""
	echo "Synopsis:"
	echo "  Script to filter GHDL analyze (-a) outputs."
	echo ""
	echo "Usage:"
	echo "  filter.analyze.sh [-v][-d] [--help] [--indent <pattern>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help             Print this help page."
	echo ""
	echo "Common options:"
	echo "  -v --verbose          Print verbose messages."
	echo "  -d --debug            Print debug messages."
	echo "  -i --indent <pattern> Indent all lines by this pattern."
	echo ""
	exit $COMMAND
fi


# States
ST_Common=1
ST_Exception=2

# Current state
State=0

# Counters
Counter_Warning=0
Counter_Error=0
Counter_Fatal=0

while read -r line
do
	line=${line//\\//}
	case $State in
		0|$ST_Common)
			if [[ $line = *"ghdl1-llvm"* ]]; then
				echo -e "$INDENT${ANSI_DARK_GRAY}$line${ANSI_NOCOLOR}"
			elif [[ $line = *":"*":"*":warning: "* ]]; then
				let Counter_Warning++
				echo -e "$INDENT${ANSI_YELLOW}WARNING:${ANSI_NOCOLOR} $line"
			elif [[ $line = *":"*":"*":note: "* ]]; then
				echo -e "$INDENT${ANSI_CYAN}NOTE:${ANSI_NOCOLOR} $line"
			elif [[ $line = *":"*":"*": "* ]]; then
				let Counter_Error++
				echo -e "$INDENT${ANSI_RED}ERROR:${ANSI_NOCOLOR} $line"
			elif [[ $line = *"compilation error"* ]]; then
				echo -e "$INDENT${ANSI_RED}FATAL:${ANSI_NOCOLOR} $line"
			elif [[ ${line:0:66} == "******************** GHDL Bug occurred ***************************" ]]; then
				State=$ST_Exception
				Counter_Fatal=1
				echo -e "$INDENT${ANSI_MAGENTA}$line${ANSI_NOCOLOR}"
			else
				if [[ $(printf '%d\n' "'$line") == "13" ]]; then
					echo ""
				else
					echo -e "${ANSI_RED}SCRIPT ERROR: Unfiltered line${ANSI_NOCOLOR}\n$line"
				fi
			fi
		;;
		$ST_Exception)
			echo -e "$INDENT${ANSI_MAGENTA}$line${ANSI_NOCOLOR}"
			if [[ ${line:0:66} == "******************************************************************" ]]; then
				State=$ST_Common
			fi
		;;
	esac
done < "/dev/stdin"

exit $(( ( $Counter_Fatal != 0 ? $Counter_Fatal*4+3 : ( $Counter_Error != 0 ? $Counter_Error*4+2 : ( $Counter_Warning != 0 ? $Counter_Warning*4+1 : 0 ))) ))
