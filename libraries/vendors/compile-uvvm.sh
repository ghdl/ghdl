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
#	Copyright (C) 2015-2017 Patrick Lehmann - Dresden, Germany
#	Copyright (C) 2017 Patrick Lehmann - Freiburg, Germany
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
if greadlink --version > /dev/null 2>&1 ; then
		READLINK=greadlink
else
		READLINK=readlink
fi

# save working directory
WorkingDir=$(pwd)
ScriptDir="$(dirname $0)"
ScriptDir="$($READLINK -f $ScriptDir)"

# source configuration file from GHDL's 'vendors' library directory
if [ -f $ScriptDir/../ansi_color.sh ]; then
		. $ScriptDir/../ansi_color.sh
fi
. $ScriptDir/config.sh
. $ScriptDir/shared.sh

uvvm_pkgs="uvvm_util
 uvvm_vvc_framework"

uvvm_vips="bitvis_vip_sbi
 bitvis_vip_avalon_mm
 bitvis_vip_axilite
 bitvis_vip_axistream
 bitvis_vip_gpio
 bitvis_vip_i2c
 bitvis_vip_spi
 bitvis_vip_uart
 bitvis_vip_wishbone

 bitvis_vip_clock_generator
 bitvis_vip_scoreboard
"

# command line argument processing
NO_COMMAND=1
SUPPRESS_WARNINGS=0
HALT_ON_ERROR=0
GHDLBinDir=""
DestDir=""
SrcDir=""
while [[ $# > 0 ]]; do
	key="$1"
	case $key in
		-c|--clean)
		CLEAN=TRUE
		NO_COMMAND=0
		;;
		-a|--all)
		COMPILE_ALL=TRUE
		NO_COMMAND=0
		;;
		--uvvm)
		COMPILE_UVVM=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip)
		COMPILE_UVVM_VIP=TRUE
		NO_COMMAND=0
		;;
		--uvvm-utilities)
		COMPILE_uvvm_util=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vvc-framework)
		COMPILE_uvvm_vvc_framework=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-avalon_mm)
		COMPILE_vip_avalon_mm=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-axi_lite)
		COMPILE_vip_axilite=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-axi_stream)
		COMPILE_vip_axistream=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-gpio)
		COMPILE_vip_gpio=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-i2c)
		COMPILE_vip_i2c=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-sbi)
		COMPILE_vip_sbi=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-spi)
		COMPILE_vip_spi=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-uart)
		COMPILE_vip_uart=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-wishbone)
		COMPILE_vip_wishbone=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-clock_generator)
		COMPILE_vip_clock_generator=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-scoreboard)
		COMPILE_vip_scoreboard=TRUE
		NO_COMMAND=0
		;;
		-h|--help)
		HELP=TRUE
		NO_COMMAND=0
		;;
		-n|--no-warnings)
		SUPPRESS_WARNINGS=1
		;;
		-H|--halt-on-error)
		HALT_ON_ERROR=1
		;;
		--ghdl)
		GHDLBinDir="$2"
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
		exit -1
		;;
	esac
	shift # past argument or value
done

# makes no sense to enable it for UVVM
SKIP_EXISTING_FILES=0

if [ $NO_COMMAND -eq 1 ]; then
	HELP=TRUE
fi

if [ "$HELP" == "TRUE" ]; then
	test $NO_COMMAND -eq 1 && echo 1>&2 -e "/n${COLORED_ERROR} No command selected."
	echo ""
	echo "Synopsis:"
	echo "  A script to compile the simulation library 'uvvm_util' for GHDL on Linux."
	echo "  A library folder 'uvvm_util/v08' will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	echo ""
	echo "Usage:"
	echo "  compile-uvvm.sh <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help             Print this help page"
	echo "  -c --clean            Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all              Compile all libraries."
	echo "     --uvvm             Compile UVVM library packages."
	echo "     --uvvm-vip         Compile UVVM Verification IPs (VIPs)."
	echo ""
	echo "Common Packages:"
	echo "     --uvvm-utilities"
	echo "     --uvvm-vvc-framework"
	echo ""
	echo "Verification IPs:"
	echo "     --uvvm-vip-avalon_mm"
	echo "     --uvvm-vip-axi_lite"
	echo "     --uvvm-vip-axi_stream"
	echo "     --uvvm-vip-gpio"
	echo "     --uvvm-vip-i2c"
	echo "     --uvvm-vip-sbi"
	echo "     --uvvm-vip-spi"
	echo "     --uvvm-vip-uart"
	echo "     --uvvm-vip-wishbone"
	echo "     --uvvm-vip-clock_generator"
	echo "     --uvvm-vip-scoreboard"
	echo ""
	echo "Library compile options:"
	echo "  -H --halt-on-error    Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "  --ghdl <GHDL bin dir> Path to GHDL's binary directory, e.g. /usr/local/bin"
	echo "  --out <dir name>      Name of the output directory, e.g. uvvm_util"
	echo "  --src <Path to UVVM>  Path to the sources."
	echo ""
	echo "Verbosity:"
	echo "  -n --no-warnings      Suppress all warnings. Show only error messages."
	echo ""
	exit 0
fi

if [ "$COMPILE_ALL" == "TRUE" ]; then
		for p in $uvvm_pkgs $uvvm_vips; do
				eval "COMPILE_$p=TRUE"
		done
fi
if [ "$COMPILE_UVVM" == "TRUE" ]; then
		for p in $uvvm_pkgs; do
				eval "COMPILE_$p=TRUE"
		done
fi
if [ "$COMPILE_UVVM_VIP" == "TRUE" ]; then
		for p in $uvvm_vips; do
				eval "COMPILE_$p=TRUE"
		done
fi

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
if [ "$CLEAN" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_NOCOLOR}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi

# UVVM libraries
# ==============================================================================
# compile uvvm_util packages
ERRORCOUNT=0
for Library in $uvvm_pkgs $uvvm_vips; do
		if [ x`eval "echo \\\$COMPILE_$Library"` == x"TRUE" ]; then
				VHDLVersion="v08"
				# append absolute source path
				files=`sed -e '/#/d' < $SourceDirectory/$Library/script/compile_order.txt`
				SourceFiles=()
				for File in $files; do
						SourceFiles+=("$SourceDirectory/$Library/script/$File")
				done

				GHDLCompilePackages
		fi
done
	
echo "--------------------------------------------------------------------------------"
echo -n "Compiling UVVM packages "
if [ $ERRORCOUNT -gt 0 ]; then
	echo -e $COLORED_FAILED
else
	echo -e $COLORED_SUCCESSFUL
fi
