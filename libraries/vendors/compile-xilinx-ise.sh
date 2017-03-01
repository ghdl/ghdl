#! /usr/bin/env bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	Bash Script:				Script to compile the simulation libraries from Xilinx ISE
#											for GHDL on Linux
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Xilinx ISE simulation libraries and packages
#
# ==============================================================================
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

# source configuration file from GHDL's 'vendors' library directory
. $ScriptDir/../ansi_color.sh
. $ScriptDir/config.sh
. $ScriptDir/shared.sh

# command line argument processing
NO_COMMAND=1
SKIP_EXISTING_FILES=0
SKIP_LARGE_FILES=0
SUPPRESS_WARNINGS=0
HALT_ON_ERROR=0
VHDLStandard=93
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
		--unisim)
		COMPILE_UNISIM=TRUE
		NO_COMMAND=0
		;;
		--unimacro)
		COMPILE_UNIMACRO=TRUE
		NO_COMMAND=0
		;;
		--simprim)
		COMPILE_SIMPRIM=TRUE
		NO_COMMAND=0
		;;
		--corelib)
		COMPILE_CORELIB=TRUE
		NO_COMMAND=0
		;;
		--secureip)
		COMPILE_SECUREIP=TRUE
		;;
		-h|--help)
		HELP=TRUE
		NO_COMMAND=0
		;;
		-s|--skip-existing)
		SKIP_EXISTING_FILES=1
		;;
		-S|--skip-largefiles)
		SKIP_LARGE_FILES=1
		;;
		-n|--no-warnings)
		SUPPRESS_WARNINGS=1
		;;
		-H|--halt-on-error)
		HALT_ON_ERROR=1
		;;
		--vhdl93)
		VHDLStandard=93
		;;
		--vhdl2008)
		VHDLStandard=2008
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

if [ $NO_COMMAND -eq 1 ]; then
	HELP=TRUE
fi

if [ "$HELP" == "TRUE" ]; then
	test $NO_COMMAND -eq 1 && echo 1>&2 -e "\n${COLORED_ERROR} No command selected."
	echo ""
	echo "Synopsis:"
	echo "  A script to compile the Xilinx ISE simulation libraries for GHDL on Linux."
	echo "  One library folder 'lib/v??' per VHDL library will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	echo ""
	echo "Usage:"
	echo "  compile-xilinx-ise.sh <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help             Print this help page"
	echo "  -c --clean            Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all              Compile all Xilinx simulation libraries."
	echo "     --unisim           Compile the unisim library."
	echo "     --unimacro         Compile the unimacro library."
	echo "     --simprim          Compile the simprim library."
	echo "     --corelib          Compile the corelib library."
	echo "     --secureip         Compile the secureip library."
	echo ""
	echo "Library compile options:"
	echo "     --vhdl93           Compile the libraries with VHDL-93."
	echo "     --vhdl2008         Compile the libraries with VHDL-2008."
	echo "  -s --skip-existing    Skip already compiled files (an *.o file exists)."
	echo "  -S --skip-largefiles  Don't compile large entities like DSP and PCIe primitives."
	echo "  -H --halt-on-error    Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "  --ghdl <GHDL bin dir> Path to GHDL's binary directory, e.g. /usr/local/bin"
	echo "  --out <dir name>      Name of the output directory, e.g. xilinx-ise"
	echo "  --src <Path to lib>   Path to the sources, e.g. /opt/Xilinx/14.7/ISE_DS/ISE/vhdl/src"
	echo ""
	echo "Verbosity:"
	echo "  -n --no-warnings        Suppress all warnings. Show only error messages."
	echo ""
	exit 0
fi

if [ "$COMPILE_ALL" == "TRUE" ]; then
	COMPILE_UNISIM=TRUE
	COMPILE_UNIMACRO=TRUE
	COMPILE_SIMPRIM=TRUE
	COMPILE_CORELIB=TRUE
	COMPILE_SECUREIP=TRUE
fi

if [ $VHDLStandard -eq 2008 ]; then
	echo -e "${ANSI_RED}Not all Xilinx primitives are VHDL-2008 compatible! Setting HALT_ON_ERROR to FALSE.${ANSI_NOCOLOR}"
	HALT_ON_ERROR=0
fi


DefaultDirectories=("/opt/Xilinx" "/opt/xilinx")
if [ ! -z $XILINX ]; then
	EnvSourceDir=$XILINX/${SourceDirectories[XilinxISE]}
else
	for DefaultDir in ${DefaultDirectories[@]}; do
		for Major in 14 13; do
			for Minor in 7 6 5 4 3 2 1 0; do
				Dir=$DefaultDir/${Major}.${Minor}/ISE_DS/ISE
				if [ -d $Dir ]; then
					EnvSourceDir=$Dir/${SourceDirectories[XilinxISE]}
					break 3
				fi
			done
		done
	done
fi

# -> $SourceDirectories
# -> $DestinationDirectories
# -> $SrcDir
# -> $EnvSourceDir
# -> $DestDir
# -> $GHDLBinDir
# <= $SourceDirectory
# <= $DestinationDirectory
# <= $GHDLBinary
SetupDirectories XilinxISE "Xilinx ISE"

# create "xilinx-ise" directory and change to it
# => $DestinationDirectory
CreateDestinationDirectory
cd $DestinationDirectory


# -> $SUPPRESS_WARNINGS
# <= $GRC_COMMAND
SetupGRCat


# -> $VHDLStandard
# <= $VHDLVersion
# <= $VHDLStandard
# <= $VHDLFlavor
GHDLSetup

# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)

GHDL_PARAMS=(${GHDL_OPTIONS[@]})
GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard -P$DestinationDirectory)


STOPCOMPILING=0
ERRORCOUNT=0

# Cleanup directory
# ==============================================================================
if [ "$CLEAN" == "TRUE" ]; then
	echo 1>&2 -e "${COLORED_ERROR} '--clean' is not implemented!"
	exit -1
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_NOCOLOR}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi

# Library unisim
# ==============================================================================
# compile unisim packages
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_UNISIM" == "TRUE" ]; then
	Library="unisim"
	Files=(
		${Library}s/unisim_VPKG.vhd
		${Library}s/unisim_VCOMP.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile unisim primitives
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_UNISIM" == "TRUE" ]; then
	Library="unisim"
	SourceFiles=()
	while IFS= read -r File; do
		SourceFiles+=("$SourceDirectory/${Library}s/primitive/$File")
	done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/${Library}s/primitive/vhdl_analyze_order")

	GHDLCompileLibrary
fi

# compile unisim secureip primitives
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_UNISIM" == "TRUE" ] && [ "$COMPILE_SECUREIP" == "TRUE" ]; then
	Library="secureip"
	SourceFiles=()
	while IFS= read -r File; do
		SourceFiles+=("$SourceDirectory/unisims/$Library/$File")
	done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/unisims/$Library/vhdl_analyze_order")

	GHDLCompileLibrary
fi

# Library unimacro
# ==============================================================================
# compile unimacro packages
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_UNIMACRO" == "TRUE" ]; then
	Library="unimacro"
	Files=(
		$Library/unimacro_VCOMP.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi
	
# compile unimacro macros
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_UNIMACRO" == "TRUE" ]; then
	Library="unimacro"
	SourceFiles=($(LC_COLLATE=C ls $SourceDirectory/$Library/*_MACRO.vhd))

	GHDLCompileLibrary
fi

# Library simprim
# ==============================================================================
# compile simprim packages
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_SIMPRIM" == "TRUE" ]; then
	Library="simprim"
	Files=(
		${Library}s/simprim_Vpackage.vhd
		${Library}s/simprim_Vcomponents.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile simprim primitives
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_SIMPRIM" == "TRUE" ]; then
	Library="simprim"
	SourceFiles=()
	while IFS= read -r File; do
		SourceFiles+=("$SourceDirectory/${Library}s/primitive/other/$File")
	done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/${Library}s/primitive/other/vhdl_analyze_order")

	GHDLCompileLibrary
fi

# compile simprim secureip primitives
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_SIMPRIM" == "TRUE" ] && [ "$COMPILE_SECUREIP" == "TRUE" ]; then
	Library="secureip"
	SourceFiles=()
	while IFS= read -r File; do
		SourceFiles+=("$SourceDirectory/simprims/$Library/other/$File")
	done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/simprims/$Library/other/vhdl_analyze_order")

	GHDLCompileLibrary
fi

# Library corelib
# ==============================================================================
# compile corelib packages
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_CORELIB" == "TRUE" ]; then
	Library="xilinxcorelib"
	
	# append absolute source path
	SourceFiles=()
	while IFS= read -r File; do
		SourceFiles+=("$SourceDirectory/XilinxCoreLib/$File")
	done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/XilinxCoreLib/vhdl_analyze_order")

	GHDLCompilePackages
fi

	
echo "--------------------------------------------------------------------------------"
echo -n "Compiling Xilinx ISE libraries "
if [ $ERRORCOUNT -gt 0 ]; then
	echo -e $COLORED_FAILED
else
	echo -e $COLORED_SUCCESSFUL
fi
