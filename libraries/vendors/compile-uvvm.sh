#! /usr/bin/env bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	Bash Script:				Script to compile the UVVM library for GHDL on Linux
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all UVVM packages 
#
# ==============================================================================
#	Copyright (C) 2017-2019 Patrick Lehmann - Boetzingen, Germany
#	Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
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
# work around for Darwin (Mac OS)
READLINK=readlink; if [[ $(uname) == "Darwin" ]]; then READLINK=greadlink; fi

# save working directory
WorkingDir=$(pwd)
ScriptDir="$(dirname $0)"
ScriptDir="$($READLINK -f $ScriptDir)"

source $ScriptDir/../../dist/ansi_color.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading Bash utilities.${ANSI_NOCOLOR}"    ; exit 1; fi


# command line argument processing
COMMAND=1
CLEAN=0
COMPILE_UVVM=0
COMPILE_UVVM_UTILITIES=0
COMPILE_UVVM_VVC_FRAMEWORK=0
COMPILE_UVVM_VIP=0
COMPILE_UVVM_VIP_AVALON_MM=0
COMPILE_UVVM_VIP_AXILITE=0
COMPILE_UVVM_VIP_AXISTREAM=0
COMPILE_UVVM_VIP_CLOCK_GENERATOR=0
COMPILE_UVVM_VIP_GPIO=0
COMPILE_UVVM_VIP_I2C=0
COMPILE_UVVM_VIP_SBI=0
COMPILE_UVVM_VIP_SCOREBOARD=0
COMPILE_UVVM_VIP_SPI=0
COMPILE_UVVM_VIP_UART=0
VERBOSE=0
DEBUG=0
FILTERING=0  # TODO: 1
SUPPRESS_WARNINGS=0
HALT_ON_ERROR=0
DestDir=""
SrcDir=""
while [[ $# > 0 ]]; do
	key="$1"
	case $key in
		-c|--clean)
			COMMAND=3
			CLEAN=1
			;;
		-a|--all)
			COMMAND=2
			;;
		--uvvm)
			COMMAND=3
			COMPILE_UVVM=1
			;;
		--uvvm-vip)
			COMMAND=3
			COMPILE_UVVM_VIP=1
			;;
		--uvvm-utilities)
			COMMAND=3
			COMPILE_UVVM_UTILITIES=1
			;;
		--uvvm-vvc-framework)
			COMMAND=3
			COMPILE_UVVM_VVC_FRAMEWORK=1
			;;
		--uvvm-vip-avalon_mm)
			COMMAND=3
			COMPILE_UVVM_VIP_AVALON_MM=1
			;;
		--uvvm-vip-axi_lite)
			COMMAND=3
			COMPILE_UVVM_VIP_AXILITE=1
			;;
		--uvvm-vip-axi_stream)
			COMMAND=3
			COMPILE_UVVM_VIP_AXISTREAM=1
			;;
		--uvvm-vip-clock)
			COMMAND=3
			COMPILE_UVVM_VIP_CLOCK_GENERATOR=1
			;;
		--uvvm-vip-gpio)
			COMMAND=3
			COMPILE_UVVM_VIP_GPIO=1
			;;
		--uvvm-vip-i2c)
			COMMAND=3
			COMPILE_UVVM_VIP_I2C=1
			;;
		--uvvm-vip-sbi)
			COMMAND=3
			COMPILE_UVVM_VIP_SBI=1
			;;
		--uvvm-vip-spi)
			COMMAND=3
			COMPILE_UVVM_VIP_SPI=1
			;;
		--uvvm-vip-scoreboard)
			COMMAND=3
			COMPILE_UVVM_VIP_SCOREBOARD=1
			;;
		--uvvm-vip-uart)
			COMMAND=3
			COMPILE_UVVM_VIP_UART=1
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
		-n|--no-filter)
			FILTERING=0
			;;
		-N|--no-warnings)
			SUPPRESS_WARNINGS=1
			;;
		-H|--halt-on-error)
			HALT_ON_ERROR=1
			;;
		--ghdl)
			GHDL="$2"				# overwrite a potentially existing GHDL environment variable
			shift						# skip argument
			;;
		--src)
			SrcDir="$2"
			shift						# skip argument
			;;
		--out)
			DestDir="$2"
			shift						# skip argument
			;;
		*)		# unknown option
			echo 1>&2 -e "${COLORED_ERROR} Unknown command line option '$key'.${ANSI_NOCOLOR}"
			exit 1
			;;
	esac
	shift # parsed argument or value
done

# makes no sense to enable it for UVVM
SKIP_EXISTING_FILES=0

if [ $COMMAND -le 1 ]; then
	test $COMMAND -eq 1 && echo 1>&2 -e "\n${COLORED_ERROR} No command selected.${ANSI_NOCOLOR}"
	echo ""
	echo "Synopsis:"
	echo "  A script to compile the simulation library 'uvvm' for GHDL on Linux."
	echo "  A library folder 'uvvm/v08' will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default parameters."
	echo ""
	echo "Usage:"
	echo "  compile-uvvm.sh [<verbosity>] <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help                 Print this help page"
	echo "  -c --clean                Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all                  Compile all libraries."
	echo "     --uvvm                 Compile UVVM library packages."
	echo "     --uvvm-vip             Compile UVVM Verification IPs (VIPs)."
	echo ""
	echo "Common Packages:"
	echo "     --uvvm-utilities      UVVM utilities."
	echo "     --uvvm-vvc-framework  VHDL Verification Component (VCC) framework."
	echo ""
	echo "Verification IPs:"
	echo "     --uvvm-vip-avalon_mm  Altera/Intel Avalon Memory Mapped"
	echo "     --uvvm-vip-axi_lite   ARM AMBA AXI4 Lite"
	echo "     --uvvm-vip-axi_stream ARM AMBA AXI4 Stream"
	echo "     --uvvm-vip-clock      Clock generator"
	echo "     --uvvm-vip-gpio       General Purpose Input/Output (GPIO)"
	echo "     --uvvm-vip-i2c        Inter-Integrated Circuit (I²C)"
	echo "     --uvvm-vip-sbi        Simple Bus Interface"
	echo "     --uvvm-vip-scoreboard Scoreboard"
	echo "     --uvvm-vip-spi        Serial Peripheral Interface"
	echo "     --uvvm-vip-uart       Universal Asynchronous Receiver Transmitter (UART)"
	echo ""
	echo "Library compile options:"
	echo "  -H --halt-on-error        Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "     --ghdl <GHDL binary>   Path to GHDL's executable, e.g. /usr/local/bin/ghdl"
	echo "     --out <dir name>       Name of the output directory, e.g. uvvm_util"
	echo "     --src <Path to UVVM>   Path to the sources."
	echo ""
	echo "Verbosity:"
	echo "  -v --verbose              Print verbose messages."
	echo "  -d --debug                Print debug messages."
#	echo "  -n --no-filter            Disable output filtering scripts."
	echo "  -N --no-warnings          Suppress all warnings. Show only error messages."
	echo ""
	exit $COMMAND
fi

if [[ $COMMAND -eq 2 ]]; then
	COMPILE_UVVM=1
	COMPILE_UVVM_VIP=1
fi
if [[ $COMPILE_UVVM -eq 1 ]]; then
	COMPILE_UVVM_UTILITIES=1
	COMPILE_UVVM_VVC_FRAMEWORK=1
fi
if [[ $COMPILE_UVVM_VIP -eq 1 ]]; then
	COMPILE_UVVM_VIP_AVALON_MM=1
	COMPILE_UVVM_VIP_AXILITE=1
	COMPILE_UVVM_VIP_AXISTREAM=1
	COMPILE_UVVM_VIP_CLOCK_GENERATOR=1
	COMPILE_UVVM_VIP_GPIO=1
	COMPILE_UVVM_VIP_I2C=1
	COMPILE_UVVM_VIP_SBI=1
	COMPILE_UVVM_VIP_SCOREBOARD=1
	COMPILE_UVVM_VIP_SPI=1
	COMPILE_UVVM_VIP_UART=1
fi


# source configuration file from GHDL's 'vendors' library directory
source $ScriptDir/config.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading configuration.${ANSI_NOCOLOR}"     ; exit 1; fi
source $ScriptDir/shared.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading further procedures.${ANSI_NOCOLOR}"; exit 1; fi

# -> $SourceDirectories
# -> $DestinationDirectories
# -> $SrcDir
# -> $DestDir
# -> $GHDLBinDir
# <= $SourceDirectory
# <= $DestinationDirectory
# <= $GHDLBinary
SetupDirectories UVVM "UVVM"

# create "uvvm_util" directory and change to it
# => $DestinationDirectory
CreateDestinationDirectory
cd $DestinationDirectory


# => $SUPPRESS_WARNINGS
# <= $GRC_COMMAND
SetupGRCat


# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)

# create a set of GHDL parameters
GHDL_PARAMS=(${GHDL_OPTIONS[@]})
GHDL_PARAMS+=(--std=08 -P$DestinationDirectory)

# Cleanup directory
# ==============================================================================
if [[ $CLEAN -eq 1 ]]; then
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_NOCOLOR}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi

# UVVM libraries
# ==============================================================================
test $VERBOSE -eq 1 && echo -e "  ${ANSI_GRAY}Reading compile order files...${ANSI_NOCOLOR}"
	
# compile uvvm_util packages
if [[ $COMPILE_UVVM_UTILITIES -eq 1 ]]; then
	UVVM_UTIL_VHDLVersion="v08"
	UVVM_UTIL_LibraryPath="uvvm_util"
	UVVM_UTIL_Files=()
	
	test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Reading compile order from '$SourceDirectory/$UVVM_UTIL_LibraryPath/script/compile_order.txt'${ANSI_NOCOLOR}"

	while IFS= read -r File; do
		if [[ ${File:0:2} == "# " ]]; then
			if [[ ${File:2:7} == "library" ]]; then
				UVVM_UTIL_LibraryName=${File:10:-1}
			fi
		else
			UVVM_UTIL_Files+=("${File:3:-1}")
		fi
	done < <(cat "$SourceDirectory/$UVVM_UTIL_LibraryPath/script/compile_order.txt")
	
	if [[ $DEBUG -eq 1 ]]; then
		echo -e "    ${ANSI_DARK_GRAY}VHDL Library name: $UVVM_UTIL_LibraryName${ANSI_NOCOLOR}"
		for File in ${UVVM_UTIL_Files[*]}; do
			echo -e "      ${ANSI_DARK_GRAY}$File${ANSI_NOCOLOR}"
		done
	fi
fi

# compile uvvm_vvc_framework packages
if [[ $COMPILE_UVVM_VVC_FRAMEWORK -eq 1 ]]; then
	UVVM_VVC_FRAMEWORK_VHDLVersion="v08"
	UVVM_VVC_FRAMEWORK_LibraryPath="uvvm_vvc_framework"
	UVVM_VVC_FRAMEWORK_Files=()

	test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Reading compile order from '$SourceDirectory/$UVVM_UTIL_LibraryPath/script/compile_order.txt'${ANSI_NOCOLOR}"

	while IFS= read -r File; do
		if [[ ${File:0:2} == "# " ]]; then
			if [[ ${File:2:7} == "library" ]]; then
				UVVM_VVC_FRAMEWORK_LibraryName=${File:10:-1}
			fi
		else
			UVVM_VVC_FRAMEWORK_Files+=("${File:3:-1}")
		fi
	done < <(cat "$SourceDirectory/$UVVM_VVC_FRAMEWORK_LibraryPath/script/compile_order.txt")
	
	if [[ $DEBUG -eq 1 ]]; then
		echo -e "    ${ANSI_DARK_GRAY}VHDL Library name: $UVVM_VVC_FRAMEWORK_LibraryName${ANSI_NOCOLOR}"
		for File in ${UVVM_VVC_FRAMEWORK_Files[*]}; do
			echo -e "      ${ANSI_DARK_GRAY}$File${ANSI_NOCOLOR}"
		done
	fi
fi


# Verification IPs
# ==============================================================================
VIPNames=()

while IFS= read -r VIPDirectory; do
	LibraryPath=$(basename "$VIPDirectory")
	x="${LibraryPath%%_*}"
	pos=${#x}+1
	l=${LibraryPath:$pos}
	VIPName=${l^^}
	
	test $VERBOSE -eq 1 && echo -e "  ${ANSI_GRAY}Found VIP '$VIPName' in '$LibraryPath'.${ANSI_NOCOLOR}"
	test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Reading compile order from '$SourceDirectory/$LibraryPath/script/compile_order.txt'${ANSI_NOCOLOR}"

	Files=()
	
	while IFS= read -r File; do
		if [[ ${File:0:2} == "# " ]]; then
			if [[ ${File:2:7} == "library" ]]; then
				LibraryName=${File:10:-1}
			fi
		else
			Files+=("${File:3:-1}")
		fi
	done < <(cat "$SourceDirectory/$LibraryPath/script/compile_order.txt")
	
	if [[ $DEBUG -eq 1 ]]; then
		echo -e "    ${ANSI_DARK_GRAY}VHDL Library name: $LibraryName${ANSI_NOCOLOR}"
		
		for File in ${Files[*]}; do
			echo -e "      ${ANSI_DARK_GRAY}$File${ANSI_NOCOLOR}"
		done
	fi
	
	declare "${VIPName}_VHDLVersion"="v08"
	declare "${VIPName}_LibraryName"=$LibraryName
	declare "${VIPName}_LibraryPath"=$LibraryPath
	
	declare -n FilesRef="${VIPName}_Files"
	FilesRef=( "${Files[@]}" )
	
	VIPNames+=("$VIPName")
done < <(find $SourceDirectory/*vip* -type d -prune)


if [[ $COMPILE_UVVM_VVC_FRAMEWORK -eq 1 ]]; then
	Libraries="UVVM_VVC_FRAMEWORK $Libraries"
fi
if [[ $COMPILE_UVVM_UTILITIES -eq 1 ]]; then
	Libraries="UVVM_UTIL $Libraries"
fi	

for VIPName in ${VIPNames[*]}; do
	VarName="COMPILE_UVVM_${VIPName}"
	if [[ ${!VarName} -eq 1 ]]; then
		Libraries="$Libraries $VIPName"
	fi
done

if [[ $Libraries != "" ]]; then
	Compile "$SourceDirectory" "$Libraries"
	
	echo "--------------------------------------------------------------------------------"
	echo -n "Compiling UVVM packages "
	if [[ $ERRORCOUNT -gt 0 ]]; then
		echo -e $COLORED_FAILED
	else
		echo -e $COLORED_SUCCESSFUL
	fi
fi
