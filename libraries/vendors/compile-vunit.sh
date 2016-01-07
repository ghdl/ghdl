#! /bin/bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Bash Script:				Script to compile the VUnit library for GHDL on Linux
# 
#	Authors:						Patrick Lehmann
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all VUnit packages 
#
# ==============================================================================
#	Copyright (C) 2015 Patrick Lehmann
#	
#	GHDL is free software; you can redistribute it and/or modify it under
#	the terms of the GNU General Public License as published by the Free
#	Software Foundation; either version 2, or (at your option) any later
#	version.
#	
#	GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#	WARRANTY; without even the implied warranty of MERCHANTABILITY or
#	FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#	for more details.
#	
#	You should have received a copy of the GNU General Public License
#	along with GHDL; see the file COPYING.  If not, write to the Free
#	Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#	02111-1307, USA.
# ==============================================================================

# ---------------------------------------------
# save working directory
WorkingDir=$(pwd)
ScriptDir="$(dirname $0)"
ScriptDir="$(realpath $ScriptDir)"

# source configuration file from GHDL's 'vendors' library directory
source $ScriptDir/config.sh
source $ScriptDir/shared.sh

NO_COMMAND=TRUE

# command line argument processing
while [[ $# > 0 ]]; do
	key="$1"
	case $key in
		-c|--clean)
		CLEAN=TRUE
		NO_COMMAND=FALSE
		;;
		-a|--all)
		ALL=TRUE
		NO_COMMAND=FALSE
		;;
		-s|--skip-existing)
		SKIP_EXISTING_FILES=TRUE
		;;
		-n|--no-warnings)
		SUPPRESS_WARNINGS=TRUE
		;;
		-H|--halt-on-error)
		HALT_ON_ERROR=TRUE
		;;
#		-v|--verbose)
#		VERBOSE=TRUE
#		;;
		-h|--help)
		HELP=TRUE
		NO_COMMAND=FALSE
		;;
		*)		# unknown option
		UNKNOWN_OPTION=TRUE
		;;
	esac
	shift # past argument or value
done

if [ "$NO_COMMAND" == "TRUE" ]; then
	HELP=TRUE
fi

if [ "$UNKNOWN_OPTION" == "TRUE" ]; then
	echo -e $COLORED_ERROR "Unknown command line option.${ANSI_RESET}"
	exit -1
elif [ "$HELP" == "TRUE" ]; then
	if [ "$NO_COMMAND" == "TRUE" ]; then
		echo -e $COLORED_ERROR " No command selected."
	fi
	echo ""
	echo "Synopsis:"
	echo "  Script to compile the simulation library VUnit for GHDL on Linux"
	echo ""
	echo "Usage:"
	echo "  compile-vunit.sh <common command>|<library> [<options>]"
#         [-v] [-c] [--all] [-s|--skip-existing] [-n|--no-warnings]
	echo ""
	echo "Common commands:"
	echo "  -h --help             Print this help page"
	echo "  -c --clean            Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all              Compile all packages."
	echo ""
	echo "Library compile options:"
	echo "  -s --skip-existing    Skip already compiled files (an *.o file exists)."
	echo "  -H --halt-on-error    Halt on error(s)."
	echo ""
	echo "Verbosity:"
#	echo "  -v --verbose          Print more messages"
	echo "  -n --no-warnings      Suppress all warnings. Show only error messages."
	echo ""
	exit 0
fi

if [ "$ALL" == "TRUE" ]; then
	UNISIM=TRUE
	UNIMACRO=TRUE
	SIMPRIM=TRUE
	SECUREIP=TRUE
fi

# extract data from configuration
InstallDir=${InstallationDirectory[VUnit]}
SourceDir="$InstallDir"
DestinationDir=${DestinationDirectory[VUnit]}

if [ -z $InstallDir ] || [ -z $DestinationDir ]; then
	echo -e "${COLORED_ERROR} VUnit is not configured in '$ScriptDir/config.sh'${ANSI_RESET}"
	exit -1
elif [ ! -d $SourceDir ]; then
	echo -e "${COLORED_ERROR} Path '$SourceDir' does not exist.${ANSI_RESET}"
	exit -1
fi

# set bash options
set -o pipefail

# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)

# create "vunit" directory and change to it
if [[ -d "$DestinationDir" ]]; then
	echo -e "${ANSI_YELLOW}Vendor directory '$DestinationDir' already exists.${ANSI_RESET}"
else
	echo -e "${ANSI_YELLOW}Creating vendor directory: '$DestinationDir'${ANSI_RESET}"
	mkdir "$DestinationDir"
fi
cd $DestinationDir

if [ -z "$(which grcat)" ]; then
	# if grcat (generic colourizer) is not installed, use a dummy pipe command like 'cat'
	GRC_COMMAND="cat"
else
	if [ "$SUPPRESS_WARNINGS" == "TRUE" ]; then
		GRC_COMMAND="grcat $ScriptDir/ghdl.skipwarning.grcrules"
	else
		GRC_COMMAND="grcat $ScriptDir/ghdl.grcrules"
	fi
fi

ERRORCOUNT=0

# Cleanup directory
# ==============================================================================
if [ "$CLEAN" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_RESET}"
	rm *.o 2> /dev/null
fi

# Library vunit_lib
# ==============================================================================
# compile vunit packages
echo -e "${ANSI_YELLOW}Compiling library 'vunit_lib' ...${ANSI_RESET}"
GHDL_PARAMS=(${GHDL_OPTIONS[@]})
GHDL_PARAMS+=(--std=08)
Files=(
	$SourceDir/vhdl/run/src/stop_api.vhd
	$SourceDir/vhdl/vhdl/src/lib/std/textio.vhd
	$SourceDir/vhdl/vhdl/src/lang/lang.vhd
	$SourceDir/vhdl/com/src/com_types.vhd
	$SourceDir/vhdl/run/src/stop_body_2008.vhd
	$SourceDir/vhdl/com/src/com_api.vhd
	$SourceDir/vhdl/string_ops/src/string_ops.vhd
	$SourceDir/vhdl/path/src/path.vhd
	$SourceDir/vhdl/logging/src/log_types.vhd
	$SourceDir/vhdl/logging/src/log_formatting.vhd
	$SourceDir/vhdl/logging/src/log_special_types200x.vhd
	$SourceDir/vhdl/array/src/array_pkg.vhd
	$SourceDir/vhdl/logging/src/log_base_api.vhd
	$SourceDir/vhdl/logging/src/log_base.vhd
	$SourceDir/vhdl/logging/src/log_api.vhd
	$SourceDir/vhdl/logging/src/log.vhd
	$SourceDir/vhdl/check/src/check_types.vhd
	$SourceDir/vhdl/check/src/check_special_types200x.vhd
	$SourceDir/vhdl/check/src/check_base_api.vhd
	$SourceDir/vhdl/check/src/check_base.vhd
	$SourceDir/vhdl/check/src/check_api.vhd
	$SourceDir/vhdl/check/src/check.vhd
	$SourceDir/vhdl/dictionary/src/dictionary.vhd
	$SourceDir/vhdl/run/src/run_types.vhd
	$SourceDir/vhdl/run/src/run_special_types200x.vhd
	$SourceDir/vhdl/run/src/run_base_api.vhd
	$SourceDir/vhdl/run/src/run_base.vhd
	$SourceDir/vhdl/run/src/run_api.vhd
	$SourceDir/vhdl/run/src/run.vhd
	$SourceDir/vhdl/vunit_run_context.vhd
	$SourceDir/vhdl/vunit_context.vhd
	$SourceDir/vhdl/com/src/com_std_codec_builder.vhd
	$SourceDir/vhdl/com/src/com_debug_codec_builder.vhd
	$SourceDir/vhdl/com/src/com_string.vhd
	$SourceDir/vhdl/com/src/com_codec_api.vhd
	$SourceDir/vhdl/com/src/com_codec.vhd
	$SourceDir/vhdl/com/src/com.vhd
	$SourceDir/vhdl/com/src/com_context.vhd
)
for File in ${Files[@]}; do
	FileName=$(basename "$File")
	if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
		echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
	else
		echo -e "${ANSI_CYAN}Analyzing package '$File'${ANSI_RESET}"
		ghdl -a ${GHDL_PARAMS[@]} --work=vunit_lib "$File" 2>&1 | $GRC_COMMAND
		if [ $? -ne 0 ]; then
			let ERRORCOUNT++
			if [ "$HALT_ON_ERROR" == "TRUE" ]; then
				break
			fi
		fi
	fi
done
	
echo "--------------------------------------------------------------------------------"
echo -n "Compiling VUnit library "
if [ $ERRORCOUNT -gt 0 ]; then
	echo -e $COLORED_FAILED
else
	echo -e $COLORED_SUCCESSFUL
fi

cd $WorkingDir
