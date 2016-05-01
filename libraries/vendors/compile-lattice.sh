#! /bin/bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Bash Script:				Script to compile the simulation libraries from Lattice
#											Diamond for GHDL on Linux
# 
#	Authors:						Patrick Lehmann and Markus Koch
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Lattice Diamond simulation libraries and packages
#
# ==============================================================================
#	Copyright (C) 2015 Patrick Lehmann and Markus Koch
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

deviceList="ec ecp ecp2 ecp3 ecp5u lptm lptm2 machxo machxo2 machxo3l sc scm xp xp2"

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
	  # All does not change the deviceList -> all selected
		NO_COMMAND=FALSE
		;;
		-s|--skip-existing)
		SKIP_EXISTING_FILES=TRUE
		;;
		-S|--skip-largefiles)
		SKIP_LARGE_FILES=TRUE
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
		-d|--device)
		shift
		deviceList="$1"
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
	echo "  Script to compile the simulation libraries from Lattice Diamond 3.6 for GHDL on Linux"
	echo ""
	echo "Usage:"
	echo "  $0 <common command>|<library> [<options>]"
#         [-v] [-c] [--unisim] [--unimacro] [--simprim] [--secureip] [-s|--skip-existing] [-S|--skip-largefiles] [-n|--no-warnings]
	echo ""
	echo "Common commands:"
	echo "  -h --help             Print this help page"
	echo "  -c --clean            Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all              Compile all Lattice simulation libraries."
	echo "  -d --device <list>    Compile only the specified device libraries. Device can be:"
  echo "                        \"$deviceList\""
	echo ""
	echo "Library compile options:"
	echo "  -s --skip-existing    Skip already compiled files (an *.o file exists)."
	echo "  -S --skip-largefiles  Don't compile large entities like DSP and PCIe primitives."
	echo "  -H --halt-on-error    Halt on error(s)."
	echo ""
	echo "Verbosity:"
#	echo "  -v --verbose          Print more messages"
	echo "  -n --no-warnings      Suppress all warnings. Show only error messages."
	echo ""
	exit 0
fi

# extract data from configuration
InstallDir=${InstallationDirectory[LatticeDiamond]}
SourceDir="$InstallDir/cae_library/simulation/vhdl"
DestinationDir=${DestinationDirectory[LatticeDiamond]}

if [ -z $InstallDir ] || [ -z $DestinationDir ]; then
	echo -e "${COLORED_ERROR} Lattice Diamond is not configured in '$ScriptDir/config.sh'${ANSI_RESET}"
	exit -1
elif [ ! -d $SourceDir ]; then
	echo -e "${COLORED_ERROR} Path '$SourceDir' does not exist.${ANSI_RESET}"
	exit -1
fi

# set bash options
set -o pipefail

# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)

# create "lattice" directory and change to it
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

STOPCOMPILING=FALSE

# Cleanup directory
# ==============================================================================
if [ "$CLEAN" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_RESET}"
	rm *.o 2> /dev/null
fi

# Lattice device libraries
# ==============================================================================
# Excluded: pmi
declare -A FileList
FileList[ec]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_LUT.vhd ORCA_MISC.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd"
FileList[ecp]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_LUT.vhd ORCA_MISC.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd"
FileList[ecp2]="ECP2_CMB.vhd ECP2_SEQ.vhd ECP2COMP.vhd ECP2_CNT.vhd ECP2_IO.vhd ECP2_LUT.vhd ECP2_MEM.vhd ECP2_MISC.vhd ECP2_MULT.vhd ECP2_SL.vhd"
FileList[ecp3]="ECP3_CMB.vhd ECP3_SEQ.vhd ECP3COMP.vhd ECP3_CNT.vhd ECP3_IO.vhd ECP3_LUT.vhd ECP3_MEM.vhd ECP3_MISC.vhd ECP3_MULT.vhd ECP3_SL.vhd"
FileList[ecp5u]="ECP5U_CMB.vhd ECP5U_SEQ.vhd ECP5UCOMP.vhd ECP5U_IO.vhd ECP5U_LUT.vhd ECP5U_MEM.vhd ECP5U_MISC.vhd ECP5U_SL.vhd gsr_pur_assign.vhd"
FileList[lptm]="MACHXO_CMB.vhd MACHXO_SEQ.vhd MACHXOCOMP.vhd MACHXO_CNT.vhd MACHXO_IO.vhd MACHXO_LUT.vhd MACHXO_MEM.vhd MACHXO_MISC.vhd"
FileList[lptm2]="MACHXO2_CMB.vhd MACHXO2_SEQ.vhd MACHXO2COMP.vhd gsr_pur_assign.vhd MACHXO2_CNT.vhd MACHXO2_IO.vhd MACHXO2_LUT.vhd MACHXO2_MEM.vhd MACHXO2_MISC.vhd"
FileList[machxo]="MACHXO_CMB.vhd MACHXO_SEQ.vhd MACHXOCOMP.vhd MACHXO_CNT.vhd MACHXO_IO.vhd MACHXO_LUT.vhd MACHXO_MEM.vhd MACHXO_MISC.vhd"
FileList[machxo2]="MACHXO2_CMB.vhd MACHXO2_SEQ.vhd MACHXO2COMP.vhd MACHXO2_CNT.vhd gsr_pur_assign.vhd MACHXO2_IO.vhd MACHXO2_LUT.vhd MACHXO2_MEM.vhd MACHXO2_MISC.vhd"
FileList[machxo3l]="MACHXO3L_CMB.vhd MACHXO3L_SEQ.vhd MACHXO3LCOMP.vhd gsr_pur_assign.vhd MACHXO3L_CNT.vhd MACHXO3L_IO.vhd MACHXO3L_LUT.vhd MACHXO3L_MEM.vhd MACHXO3L_MISC.vhd"
FileList[sc]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd ORCA_MIS.vhd ORCA_SL.vhd"
FileList[scm]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd ORCA_MIS.vhd ORCA_SL.vhd"
FileList[xp]="ORCA_CMB.vhd ORCA_SEQ.vhd ORCACOMP.vhd ORCA_LUT.vhd ORCA_MISC.vhd ORCA_CNT.vhd ORCA_IO.vhd ORCA_MEM.vhd"
FileList[xp2]="XP2_CMB.vhd XP2_SEQ.vhd XP2COMP.vhd XP2_CNT.vhd XP2_IO.vhd XP2_LUT.vhd XP2_MEM.vhd XP2_MISC.vhd XP2_MULT.vhd XP2_SL.vhd"

for device in $deviceList; do
	if [ "$STOPCOMPILING" == "FALSE" ]; then
		echo -e "${ANSI_YELLOW}Compiling library '$device' ...${ANSI_RESET}"
		GHDL_PARAMS=(${GHDL_OPTIONS[@]})
		GHDL_PARAMS+=(--ieee=synopsys --std=93c)
		currentPath="$SourceDir/$device/src"
		Files=(${FileList[$device]})
		#Files+=`find $currentPath/*.vhd`
		for File_a in ${Files[@]}; do
			File="$currentPath/$File_a"
			FileName=$(basename "$File")
			FileName="${device}_$FileName"
			if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
				echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
			else
				echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
				ghdl -a ${GHDL_PARAMS[@]} --work=$device -o "${FileName%.*}.o" "$File" 2>&1 | $GRC_COMMAND
				if [ $? -ne 0 ] && [ "$HALT_ON_ERROR" == "TRUE" ]; then
					STOPCOMPILING=TRUE
					break
				fi
			fi
		done
	fi
done

echo "--------------------------------------------------------------------------------"
echo -n "Compiling Lattice Diamond libraries "
if [ "$STOPCOMPILING" == "TRUE" ]; then
	echo -e $COLORED_FAILED
else
	echo -e $COLORED_SUCCESSFUL
fi

cd $WorkingDir
