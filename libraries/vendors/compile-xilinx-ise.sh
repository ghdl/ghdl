#! /bin/bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Bash Script:				Script to compile the simulation libraries from Xilinx ISE
#											for GHDL on Linux
# 
#	Authors:						Patrick Lehmann
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Xilinx ISE simulation libraries and packages
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
		--unisim)
		UNISIM=TRUE
		NO_COMMAND=FALSE
		;;
		--unimacro)
		UNIMACRO=TRUE
		NO_COMMAND=FALSE
		;;
		--simprim)
		SIMPRIM=TRUE
		NO_COMMAND=FALSE
		;;
		--secureip)
		SECUREIP=TRUE
		;;
		--vhdl93)
		VHDL93=TRUE
		;;
		--vhdl2008)
		VHDL2008=TRUE
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
	echo "  Script to compile the simulation libraries from Xilinx ISE for GHDL on Linux"
	echo ""
	echo "Usage:"
	echo "  compile-xilinx-ise.sh <common command>|<library> [<options>]"
#         [-v] [-c] [--unisim] [--unimacro] [--simprim] [--secureip] [-s|--skip-existing] [-S|--skip-largefiles] [-n|--no-warnings]
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
	echo "     --secureip         Compile the secureip library."
	echo ""
	echo "Library compile options:"
	echo "     --vhdl93           Compile the libraries with VHDL-93."
	echo "     --vhdl2008         Compile the libraries with VHDL-2008."
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

if [ "$ALL" == "TRUE" ]; then
	UNISIM=TRUE
	UNIMACRO=TRUE
	SIMPRIM=TRUE
	SECUREIP=TRUE
fi

if [ "$VHDL93" == "TRUE" ]; then
	VHDLStandard="93c"
	VHDLFlavor="synopsys"
elif [ "$VHDL2008" == "TRUE" ]; then
	VHDLStandard="08"
	VHDLFlavor="standard"
	echo -e "${ANSI_RED}Not all Xilinx primitives are VHDL-2008 compatible! Setting HALT_ON_ERROR to FALSE.${ANSI_RESET}"
	HALT_ON_ERROR=FALSE
else
	VHDLStandard="93c"
	VHDLFlavor="synopsys"
fi

# extract data from configuration
SourceDir=${SourceDirectory[XilinxISE]}
DestinationDir=${DestinationDirectory[XilinxISE]}

if [ -z $DestinationDir ]; then
	echo -e "${COLORED_ERROR} Xilinx ISE is not configured in '$ScriptDir/config.sh'${ANSI_RESET}"
	exit -1
elif [ ! -d $SourceDir ]; then
	echo -e "${COLORED_ERROR} Path '$SourceDir' does not exist.${ANSI_RESET}"
	exit -1
fi

# set bash options
set -o pipefail

# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)

# create "xilinx" directory and change to it
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
ERRORCOUNT=0

# Cleanup directory
# ==============================================================================
if [ "$CLEAN" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_RESET}"
	rm *.o 2> /dev/null
fi

# Library unisim
# ==============================================================================
# compile unisim packages
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$UNISIM" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'unisim' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/unisims/unisim_VPKG.vhd
		$SourceDir/unisims/unisim_VCOMP.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing package '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=unisim "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				if [ "$HALT_ON_ERROR" == "TRUE" ]; then
					STOPCOMPILING=TRUE
					break
				fi
			fi
		fi
	done
fi

# compile unisim primitives
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$UNISIM" == "TRUE" ]; then
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files="$(LC_COLLATE=C ls $SourceDir/unisims/primitive/*.vhd)"
	for File in $Files; do
		FileName=$(basename "$File")
		FileSize=($(wc -c $File))
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		elif [ "$SKIP_LARGE_FILES" == "TRUE" ] && [ ${FileSize[0]} -gt $LARGE_FILESIZE ]; then
			echo -e "${ANSI_CYAN}Skipping large '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=unisim "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				if [ "$HALT_ON_ERROR" == "TRUE" ]; then
					STOPCOMPILING=TRUE
					break
				fi
			fi
		fi
	done
fi

# compile unisim secureip primitives
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$UNISIM" == "TRUE" ] && [ "$SECUREIP" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library secureip primitives${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files="$(LC_COLLATE=C ls $SourceDir/unisims/secureip/*.vhd)"
	for File in $Files; do
		FileName=$(basename "$File")
		FileSize=($(wc -c $File))
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		elif [ "$SKIP_LARGE_FILES" == "TRUE" ] && [ ${FileSize[0]} -gt $LARGE_FILESIZE ]; then
			echo -e "${ANSI_CYAN}Skipping large '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=secureip "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				if [ "$HALT_ON_ERROR" == "TRUE" ]; then
					STOPCOMPILING=TRUE
					break
				fi
			fi
		fi
	done
fi

# Library unimacro
# ==============================================================================
# compile unimacro packages
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$UNIMACRO" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'unimacro' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/unimacro/unimacro_VCOMP.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing package '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=unimacro "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				if [ "$HALT_ON_ERROR" == "TRUE" ]; then
					STOPCOMPILING=TRUE
					break
				fi
			fi
		fi
	done
fi
	
# compile unimacro macros
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$UNIMACRO" == "TRUE" ]; then
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files="$(LC_COLLATE=C ls $SourceDir/unimacro/*_MACRO.vhd)"
	for File in $Files; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=unimacro "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				if [ "$HALT_ON_ERROR" == "TRUE" ]; then
					STOPCOMPILING=TRUE
					break
				fi
			fi
		fi
	done
fi

# Library simprim
# ==============================================================================
# compile simprim packages
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$SIMPRIM" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'simprim' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/simprims/simprim_Vpackage.vhd
		$SourceDir/simprims/simprim_Vcomponents.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing package '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=simprim "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				if [ "$HALT_ON_ERROR" == "TRUE" ]; then
					STOPCOMPILING=TRUE
					break
				fi
			fi
		fi
	done
fi

# compile simprim primitives
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$SIMPRIM" == "TRUE" ]; then
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files="$(LC_COLLATE=C ls $SourceDir/simprims/primitive/other/*.vhd)"
	for File in $Files; do
		FileName=$(basename "$File")
		FileSize=($(wc -c $File))
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		elif [ "$SKIP_LARGE_FILES" == "TRUE" ] && [ ${FileSize[0]} -gt $LARGE_FILESIZE ]; then
			echo -e "${ANSI_CYAN}Skipping large '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=simprim "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				if [ "$HALT_ON_ERROR" == "TRUE" ]; then
					STOPCOMPILING=TRUE
					break
				fi
			fi
		fi
	done
fi

# compile simprim secureip primitives
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$SIMPRIM" == "TRUE" ] && [ "$SECUREIP" == "TRUE" ]; then
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files="$(LC_COLLATE=C ls $SourceDir/simprims/secureip/other/*.vhd)"
	for File in $Files; do
		FileName=$(basename "$File")
		FileSize=($(wc -c $File))
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		elif [ "$SKIP_LARGE_FILES" == "TRUE" ] && [ ${FileSize[0]} -gt $LARGE_FILESIZE ]; then
			echo -e "${ANSI_CYAN}Skipping large '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=simprim "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ]; then
				let ERRORCOUNT++
				if [ "$HALT_ON_ERROR" == "TRUE" ]; then
					STOPCOMPILING=TRUE
					break
				fi
			fi
		fi
	done
fi
	
echo "--------------------------------------------------------------------------------"
echo -n "Compiling Xilinx ISE libraries "
if [ $ERRORCOUNT -gt 0 ]; then
	echo -e $COLORED_FAILED
else
	echo -e $COLORED_SUCCESSFUL
fi

cd $WorkingDir
