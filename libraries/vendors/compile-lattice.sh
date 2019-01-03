#! /usr/bin/env bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Markus Koch
#											Patrick Lehmann
# 
#	Bash Script:				Script to compile the simulation libraries from Lattice
#											Diamond for GHDL on Linux
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Lattice Diamond simulation libraries and packages
#
# ==============================================================================
#	Copyright (C) 2017-2019 Patrick Lehmann - Boetzingen, Germany
#	Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
#	Copyright (C) 2015-2016 Markus Koch
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
DeviceList="ec ecp ecp2 ecp3 ecp5u lptm lptm2 machxo machxo2 machxo3l sc scm xp xp2"
VERBOSE=0
DEBUG=0
FILTERING=0  # TODO: 1
SKIP_EXISTING_FILES=0
SUPPRESS_WARNINGS=0
HALT_ON_ERROR=0
VHDLStandard=93
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
		-d|--device)
			DeviceList="$2"
			shift
			COMMAND=3
			;;
		-s|--skip-existing)
			SKIP_EXISTING_FILES=1
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
			echo 1>&2 -e "${COLORED_ERROR} Unknown command line option '$1'.${ANSI_NOCOLOR}"
			exit 1
			;;
	esac
	shift # parsed argument or value
done

ERRORCOUNT=0

if [[ $COMMAND -le 1 ]]; then
	test $COMMAND -eq 1 && echo 1>&2 -e "\n${COLORED_ERROR} No command selected.${ANSI_NOCOLOR}"
	echo ""
	echo "Synopsis:"
	echo "  A script to compile the Lattice Diamond simulation libraries for GHDL on Linux."
	echo "  One library folder 'lib/v??' per VHDL library will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	echo ""
	echo "Usage:"
	echo "  compile-lattice.sh <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help                Print this help page"
	echo "  -c --clean               Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all                 Compile all Lattice simulation libraries."
	echo "  -d --device <list>       Compile only the specified device libraries. Device can be:"
  echo "                           \"$deviceList\""
	echo ""
	echo "Library compile options:"
	echo "     --vhdl93              Compile the libraries with VHDL-93."
	echo "     --vhdl2008            Compile the libraries with VHDL-2008."
	echo "  -s --skip-existing       Skip already compiled files (an *.o file exists)."
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

if [[ $VHDLStandard -eq 2008 ]]; then
	echo -e "${ANSI_RED}Not all Lattice packages are VHDL-2008 compatible! Setting HALT_ON_ERROR to FALSE.${ANSI_NOCOLOR}"
	HALT_ON_ERROR=0
fi

DefaultDirectories=("/usr/local/diamond" "/opt/Diamond" "/opt/diamond")
if [ ! -z $LSC_DIAMOND ]; then
	EnvSourceDir=$FOUNDRY/../${SourceDirectories[LatticeDiamond]}
else
	for DefaultDir in ${DefaultDirectories[@]}; do
		for Major in 3; do
			for Minor in 8 7 6 5; do
				Dir=$DefaultDir/${Major}.${Minor}_x64
				if [ -d $Dir ]; then
					EnvSourceDir=$Dir/${SourceDirectories[LatticeDiamond]}
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
SetupDirectories LatticeDiamond "Lattice Diamond"

# create "lattice" directory and change to it
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

# Lattice device libraries
# ==============================================================================
# Excluded: pmi
declare -A FileLists
FileLists[ec]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_LUT.vhd ORCA_MISC.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd"
FileLists[ecp]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_LUT.vhd ORCA_MISC.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd"
FileLists[ecp2]="ECP2_CMB.vhd ECP2_SEQ.vhd ECP2COMP.vhd ECP2_CNT.vhd ECP2_IO.vhd ECP2_LUT.vhd ECP2_MEM.vhd ECP2_MISC.vhd ECP2_MULT.vhd ECP2_SL.vhd"
FileLists[ecp3]="ECP3_CMB.vhd ECP3_SEQ.vhd ECP3COMP.vhd ECP3_CNT.vhd ECP3_IO.vhd ECP3_LUT.vhd ECP3_MEM.vhd ECP3_MISC.vhd ECP3_MULT.vhd ECP3_SL.vhd"
FileLists[ecp5u]="ECP5U_CMB.vhd ECP5U_SEQ.vhd ECP5UCOMP.vhd ECP5U_IO.vhd ECP5U_LUT.vhd ECP5U_MEM.vhd ECP5U_MISC.vhd ECP5U_SL.vhd gsr_pur_assign.vhd"
FileLists[lptm]="MACHXO_CMB.vhd MACHXO_SEQ.vhd MACHXOCOMP.vhd MACHXO_CNT.vhd MACHXO_IO.vhd MACHXO_LUT.vhd MACHXO_MEM.vhd MACHXO_MISC.vhd"
FileLists[lptm2]="MACHXO2_CMB.vhd MACHXO2_SEQ.vhd MACHXO2COMP.vhd gsr_pur_assign.vhd MACHXO2_CNT.vhd MACHXO2_IO.vhd MACHXO2_LUT.vhd MACHXO2_MEM.vhd MACHXO2_MISC.vhd"
FileLists[machxo]="MACHXO_CMB.vhd MACHXO_SEQ.vhd MACHXOCOMP.vhd MACHXO_CNT.vhd MACHXO_IO.vhd MACHXO_LUT.vhd MACHXO_MEM.vhd MACHXO_MISC.vhd"
FileLists[machxo2]="MACHXO2_CMB.vhd MACHXO2_SEQ.vhd MACHXO2COMP.vhd MACHXO2_CNT.vhd gsr_pur_assign.vhd MACHXO2_IO.vhd MACHXO2_LUT.vhd MACHXO2_MEM.vhd MACHXO2_MISC.vhd"
FileLists[machxo3l]="MACHXO3L_CMB.vhd MACHXO3L_SEQ.vhd MACHXO3LCOMP.vhd gsr_pur_assign.vhd MACHXO3L_CNT.vhd MACHXO3L_IO.vhd MACHXO3L_LUT.vhd MACHXO3L_MEM.vhd MACHXO3L_MISC.vhd"
FileLists[sc]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd ORCA_MIS.vhd ORCA_SL.vhd"
FileLists[scm]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd ORCA_MIS.vhd ORCA_SL.vhd"
FileLists[xp]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_LUT.vhd ORCA_MISC.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd"
FileLists[xp2]="XP2_CMB.vhd XP2_SEQ.vhd XP2COMP.vhd XP2_CNT.vhd XP2_IO.vhd XP2_LUT.vhd XP2_MEM.vhd XP2_MISC.vhd XP2_MULT.vhd XP2_SL.vhd"

for device in $DeviceList; do
	Library=$device
	LibraryDirectory=$DestinationDirectory/$Library/$VHDLVersion
	mkdir -p $LibraryDirectory
	cd $LibraryDirectory
	echo -e "${ANSI_YELLOW}Compiling library '$Library'...${ANSI_NOCOLOR}"

	DeviceSourceDirectory="$SourceDirectory/$device/src"
	for File in ${FileLists[$device]}; do
		File="$DeviceSourceDirectory/$File"
		FileName=$(basename "$File")
		FileName="${device}_$FileName"
		if [ $SKIP_EXISTING_FILES -eq 1 ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_NOCOLOR}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_NOCOLOR}"
			$GHDLBinary -a ${GHDL_PARAMS[@]} --work=$Library "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				test $HALT_ON_ERROR -eq 1 && break 2
			fi
		fi
	done
done

# Compile libraries
if [[ "$Libraries" != "" ]]; then
	Compile "$SourceDirectory" "${Libraries[*]}"
	
	echo "--------------------------------------------------------------------------------"
	echo -e "Compiling Lattice device libraries $(test $ERRORCOUNT -eq 0 && echo $COLORED_SUCCESSFUL || echo $COLORED_FAILED)"
else
	echo -e "${ANSI_RED}No Lattice device library selected.${ANSI_NOCOLOR}"
fi
