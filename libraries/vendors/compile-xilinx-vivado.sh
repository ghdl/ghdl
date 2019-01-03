#! /usr/bin/env bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	Bash Script:				Script to compile the simulation libraries from Xilinx Vivado
#											for GHDL on Linux
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Xilinx Vivado simulation libraries and packages
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

# Work around for Darwin (Mac OS)
READLINK=readlink; if [[ $(uname) == "Darwin" ]]; then READLINK=greadlink; fi

# Save working directory
WorkingDir=$(pwd)
ScriptDir="$(dirname $0)"
ScriptDir="$($READLINK -f $ScriptDir)"

# Source Bash utilities
source $ScriptDir/../../dist/ansi_color.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading Bash utilities.${ANSI_NOCOLOR}"    ; exit 1; fi


# Command line argument processing
COMMAND=1
CLEAN=0
COMPILE_UNISIM=0
COMPILE_UNIMACRO=0
COMPILE_UNIFAST=0
COMPILE_SECUREIP=0
VERBOSE=0
DEBUG=0
FILTERING=0  # TODO: 1
SKIP_EXISTING_FILES=0
SKIP_LARGE_FILES=0
SUPPRESS_WARNINGS=0
HALT_ON_ERROR=0
VHDLStandard=93
GHDLBinDir=""
DestDir=""
SrcDir=""
while [[ $# > 0 ]]; do
	case "$1" in
		-c|--clean)
			COMMAND=3
			CLEAN=1
			;;
		-a|--all)
			COMMAND=2
			;;
		--unisim)
			COMMAND=3
			COMPILE_UNISIM=1
			;;
		--unimacro)
			COMMAND=3
			COMPILE_UNIMACRO=1
			;;
		--unifast)
			COMMAND=3
			COMPILE_UNIFAST=1
			;;
		--with-secureip)
			COMPILE_SECUREIP=1
			;;
		-s|--skip-existing)
			SKIP_EXISTING_FILES=1
			;;
		-S|--skip-largefiles)
			SKIP_LARGE_FILES=1
			;;
		--vhdl93)
			VHDLStandard=93
			;;
		--vhdl2008)
			VHDLStandard=2008
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
			break
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
			echo 1>&2 -e "\n${COLORED_ERROR} Unknown command line option '$1'.${ANSI_NOCOLOR}"
			COMMAND=0
			break
			;;
	esac
	shift # parsed argument or value
done

ERRORCOUNT=0

if [[ $COMMAND -le 1 ]]; then
	test $COMMAND -eq 1 && echo 1>&2 -e "\n${COLORED_ERROR} No command selected.${ANSI_NOCOLOR}"
	echo ""
	echo "Synopsis:"
	echo "  A script to compile the Xilinx Vivado simulation libraries for GHDL on Linux."
	echo "  One library folder 'lib/v??' per VHDL library will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	echo ""
	echo "Usage:"
	echo "  compile-xilinx-vivado.sh <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help                Print this help page"
	echo "  -c --clean               Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all                 Compile all Xilinx simulation libraries."
	echo "     --unisim              Compile the unisim library."
	echo "     --unimacro            Compile the unimacro library."
	echo "     --unifast             Compile the unifast library."
	echo "     --secureip            Compile the secureip library."
	echo ""
	echo "Library compile options:"  
	echo "     --vhdl93              Compile the libraries with VHDL-93."
	echo "     --vhdl2008            Compile the libraries with VHDL-2008."
	echo "  -s --skip-existing       Skip already compiled files (an *.o file exists)."
	echo "  -S --skip-largefiles     Don't compile large files. Exclude *HSSI* and *HIP* files."
	echo "  -H --halt-on-error       Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "     --ghdl <GHDL binary>  Path to GHDL's executable, e.g. /usr/local/bin/ghdl"
	echo "     --out <dir name>      Name of the output directory, e.g. uvvm_util"
	echo "     --src <Path to UVVM>  Path to the sources."
	echo ""
	echo "Verbosity:"
	echo "  -v --verbose             Print verbose messages."
	echo "  -d --debug               Print debug messages."
#	echo "  -n --no-filter           Disable output filtering scripts."
	echo "  -N --no-warnings         Suppress all warnings. Show only error messages."
	echo ""
	exit $COMMAND
fi

if [[ $COMMAND -eq 2 ]]; then
	COMPILE_UNISIM=1
	COMPILE_UNIMACRO=1
	COMPILE_UNIFAST=1
	COMPILE_SECUREIP=1
fi

if [[ $VHDLStandard -eq 2008 ]]; then
	echo -e "${ANSI_RED}Not all Xilinx primitives are VHDL-2008 compatible! Setting HALT_ON_ERROR to FALSE.${ANSI_NOCOLOR}"
	HALT_ON_ERROR=0
fi


DefaultDirectories=("/opt/Xilinx/Vivado" "/opt/xilinx/Vivado")
if [ ! -z $XILINX_VIVADO ]; then
	EnvSourceDir=$XILINX_VIVADO/${SourceDirectories[XilinxVivado]}
else
	for DefaultDir in ${DefaultDirectories[@]}; do
		for Major in 2017 2016 2015 2014; do
			for Minor in 4 3 2 1; do
				Dir=$DefaultDir/${Major}.${Minor}
				if [ -d $Dir ]; then
					EnvSourceDir=$Dir/${SourceDirectories[XilinxVivado]}
					break 3
				fi
			done
		done
	done
fi

# Source configuration file from GHDL's 'vendors' library directory
source $ScriptDir/config.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading configuration.${ANSI_NOCOLOR}"     ; exit 1; fi
source $ScriptDir/shared.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading further procedures.${ANSI_NOCOLOR}"; exit 1; fi

# -> $SourceDirectories
# -> $DestinationDirectories
# -> $SrcDir
# -> $EnvSourceDir
# -> $DestDir
# <= $SourceDirectory
# <= $DestinationDirectory
SetupDirectories XilinxVivado "Xilinx Vivado"

# create "xilinx-vivado" directory and change to it
# => $DestinationDirectory
CreateDestinationDirectory
cd $DestinationDirectory


# => $SUPPRESS_WARNINGS
# <= $GRC_COMMAND
SetupGRCat


# -> $VHDLStandard
# <= $VHDLVersion
# <= $VHDLStandard
# <= $VHDLFlavor
GHDLSetup

# Define global GHDL Options
GHDL_OPTIONS=(
	-fexplicit
	-frelaxed-rules
	--no-vital-checks
	-Wbinding
	--mb-comments
)

# Create a set of GHDL parameters
GHDL_PARAMS=(${GHDL_OPTIONS[@]})
GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard -P$DestinationDirectory)


STOPCOMPILING=0

# Cleanup directory
# ==============================================================================
if [[ $CLEAN -eq 1 ]]; then
	echo 1>&2 -e "${COLORED_ERROR} '--clean' is not implemented!"
	exit 1
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
		${Library}s/retarget_VCOMP.vhd
		${Library}s/unisim_retarget_VCOMP.vhd
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

# compile unisim retarget primitives
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_UNISIM" == "TRUE" ]; then
	Library="unisim"
	SourceFiles="$(LC_COLLATE=C ls $SourceDirectory/${Library}s/retarget/*.vhd)"

	GHDLCompileLibrary
fi

# compile unisim secureip primitives
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_UNISIM" == "TRUE" ] && [ "$COMPILE_SECUREIP" == "TRUE" ]; then
	Library="secureip"
	SourceFiles="$(LC_COLLATE=C ls $SourceDirectory/unisims/$Library/*.vhd)"

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
	SourceFiles=()
	while IFS= read -r File; do
		SourceFiles+=("$SourceDirectory/${Library}/$File")
	done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/${Library}/vhdl_analyze_order")

	GHDLCompileLibrary
fi

# Library UNIFAST
# ==============================================================================
# compile unisim primitives
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_UNIFAST" == "TRUE" ]; then
	Library="unifast"
	SourceFiles=()
	while IFS= read -r File; do
		SourceFiles+=("$SourceDirectory/${Library}/primitive/$File")
	done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/${Library}/primitive/vhdl_analyze_order")

	GHDLCompileLibrary
fi

# Compile libraries
if [[ "$Libraries" != "" ]]; then
	Compile "$SourceDirectory" "${Libraries[*]}"
	
	echo "--------------------------------------------------------------------------------"
	echo -e "Compiling Xilinx Vivado libraries $(test $ERRORCOUNT -eq 0 && echo $COLORED_SUCCESSFUL || echo $COLORED_FAILED)"
else
	echo -e "${ANSI_RED}No Xilinx Vivado libraries selected.${ANSI_NOCOLOR}"
fi
