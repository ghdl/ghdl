#! /bin/bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	Bash Script:				Script to compile the OSVVM library for GHDL on Linux
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all OSVVM packages 
#
# ==============================================================================
#	Copyright (C) 2015-2016 Patrick Lehmann
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
ScriptDir="$(readlink -f $ScriptDir)"

# source configuration file from GHDL's 'vendors' library directory
source $ScriptDir/config.sh
source $ScriptDir/shared.sh

# command line argument processing
NO_COMMAND=TRUE
BinDir=""
DestDir=""
SrcDir=""
while [[ $# > 0 ]]; do
	key="$1"
	case $key in
		-c|--clean)
		CLEAN=TRUE
		NO_COMMAND=FALSE
		;;
		-a|--all)
		COMPILE_ALL=TRUE
		NO_COMMAND=FALSE
		;;
		-o|--osvvm)
		COMPILE_OSVVM=TRUE
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
		-h|--help)
		HELP=TRUE
		NO_COMMAND=FALSE
		;;
		--ghdl)
		BinDir="$2"
		shift						# past argument
		;;
		--src)
		SrcDir="$2"
		shift						# past argument
		;;
		--out)
		DestDir="$2"
		shift						# past argument
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
	echo "  A script to compile the simulation library 'OSVVM' for GHDL on Linux."
	echo "  A library folder 'osvvm/v08' will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	echo ""
	echo "Usage:"
	echo "  compile-osvvm.sh <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help              Print this help page"
	echo "  -c --clean             Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all               Compile all packages (default)."
	echo "  -o --osvvm             Compile package osvvm."
	echo ""
	echo "Library compile options:"
	echo "  -s --skip-existing     Skip already compiled files (an *.o file exists)."
	echo "  -H --halt-on-error     Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "  --ghdl <GHDL BinDir>   Path to GHDL binary directory e.g. /usr/bin."
	echo "  --out <dir name>       Name of the output directory."
	echo "  --src <Path to OSVVM>  Name of the output directory."
	echo ""
	echo "Verbosity:"
	echo "  -n --no-warnings      Suppress all warnings. Show only error messages."
	echo ""
	exit 0
fi

if [ "$COMPILE_ALL" == "TRUE" ]; then
	COMPILE_OSVVM=TRUE
fi

SourceDirectory=${InstallationDirectory[OSVVM]}
DestinationDir=${DestinationDirectory[OSVVM]}

# OSVVM source directory
# ----------------------
# If a command line argument ('--src') was passed in, use it, else use the default value
# from config.sh
if [ -n $SrcDir ]; then
	SourceDirectory=$SrcDir
fi
# OSVVM output directory
# ----------------------
# If a command line argument ('--out') was passed in, use it, else use the default value
# from config.sh
if [ -n $DestDir ]; then
	DestinationDir=$DestDir
fi
# Use GHDL binary directory from command line argument, if set
if [ -n $BinDir ]; then
	GHDLBinary=$BinDir/ghdl
	if [[ -x "$GHDLBinary" ]]; then
		echo -e "${COLORED_ERROR} GHDL not found or is not executable.${ANSI_RESET}"
		exit -1
	fi
else
	# fall back to GHDL found via PATH
	GHDLBinary=$(which ghdl)
	if [ $? -ne 0 ]; then
		echo -e "${COLORED_ERROR} No GHDL found.${ANSI_RESET}"
		echo -e "  Use adv. options '--ghdl' to set the GHDL binary directory."
		exit -1
	fi
fi

if [ -z $SourceDirectory ] || [ -z $DestinationDir ]; then
	echo -e "${COLORED_ERROR} OSVVM is not configured in '$ScriptDir/config.sh'${ANSI_RESET}"
	echo -e "  Use adv. options '--src' and '--out' or configure 'config.sh'."
	exit -1
elif [ ! -d $SourceDirectory ]; then
	echo -e "${COLORED_ERROR} Path '$SourceDir' does not exist.${ANSI_RESET}"
	exit -1
fi

# append VHDL version folder
DestinationDir=$DestinationDir/v08

# set bash options
set -o pipefail

# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)

# create "osvvm" directory and change to it
if [[ -d "$DestinationDir" ]]; then
	echo -e "${ANSI_YELLOW}Vendor directory '$DestinationDir' already exists.${ANSI_RESET}"
else
	echo -e "${ANSI_YELLOW}Creating vendor directory: '$DestinationDir'${ANSI_RESET}"
	mkdir -p "$DestinationDir"
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

# Cleanup directory
# ==============================================================================
if [ "$CLEAN" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Cleaning up directory ...${ANSI_RESET}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi

# Library osvvm
# ==============================================================================
# compile osvvm packages
if [ "$COMPILE_OSVVM" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'osvvm' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--std=08)
	Files=(
		$SourceDir/NamePkg.vhd
		$SourceDir/OsvvmGlobalPkg.vhd
		$SourceDir/TextUtilPkg.vhd
		$SourceDir/TranscriptPkg.vhd
		$SourceDir/AlertLogPkg.vhd
		$SourceDir/MemoryPkg.vhd
		$SourceDir/MessagePkg.vhd
		$SourceDir/SortListPkg_int.vhd
		$SourceDir/RandomBasePkg.vhd
		$SourceDir/RandomPkg.vhd
		$SourceDir/CoveragePkg.vhd
		$SourceDir/OsvvmContext.vhd
	)

	ERRORCOUNT=0
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing package '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=osvvm "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				if [ "$HALT_ON_ERROR" == "TRUE" ]; then
					break
				fi
			fi
		fi
	done
		
	echo "--------------------------------------------------------------------------------"
	echo -n "Compiling OSVVM library "
	if [ $ERRORCOUNT -gt 0 ]; then
		echo -e $COLORED_FAILED
	else
		echo -e $COLORED_SUCCESSFUL
	fi
fi
