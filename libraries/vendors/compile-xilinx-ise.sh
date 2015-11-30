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

# source configuration file from GHDL's 'vendors' library directory
source config.sh
source shared.sh

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
	echo "  compile-xilinx-ise.sh [-v] [-c] [-u|--unisim] [-U|--unimacro] [-s|--simprim] [-S|--secureip] [-l|--large] [--skip-existing] [--no-warnings]"
	echo ""
	echo "Commands:"
	echo "  -h --help             Print this help page"
	echo "  -c --clean            Remove all generated files"
	echo "  -a --all              Compile all Xilinx simulation libraries."
	echo "     --unisim           Compile the unisim library."
	echo "     --unimacro         Compile the unimacro library."
	echo "     --simprim          Compile the simprim library."
	echo "     --secureip         Compile the secureip library."
	echo ""
	echo "Compile options:"
	echo "  -s --skip-existing    Skip already compiled files (an *.o file exists)."
	echo "  -S --skip-largefiles  Don't compile large entities like DSP and PCIe primitives."
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
SourceDir="${InstallationDirectory[XilinxISE]}/ISE_DS/ISE/vhdl/src"
DestinationDir="${DestinationDirectory[XilinxISE]}"
ScriptDir=".."

# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)

# create "Xilinx" directory and change to it
if [[ -d "$DestinationDir" ]]; then
	echo -e "${ANSI_YELLOW}Vendor directory '$DestinationDir' already exists.${ANSI_RESET}"
else
	echo -e "${ANSI_YELLOW}Creating vendor directory: '$DestinationDir'${ANSI_RESET}"
	mkdir "$DestinationDir"
fi
cd $DestinationDir

STOPCOMPILING=TRUE

if [ "$SUPPRESS_WARNINGS" == "FALSE" ]; then
	GRCRulesFile="$ScriptDir/ghdl.grcrules"
else
	GRCRulesFile="$ScriptDir/ghdl.skipwarning.grcrules"
fi


# Cleanup directory
# ==============================================================================
if [ "$CLEAN" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_RESET}"
	rm *.o
fi

# Library unisim
# ==============================================================================
# compile unisim packages
if [ "$UNISIM" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'unisim' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=synopsys --std=93c)
	Files=(
		$SourceDir/unisims/unisim_VPKG.vhd
		$SourceDir/unisims/unisim_VCOMP.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing package '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=unisim "$File" 2>&1 | grcat $GRCRulesFile
		fi
	done
fi

# compile unisim primitives
if [ "$UNISIM" == "TRUE" ]; then
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=synopsys --std=93c)
	Files=$SourceDir/unisims/primitive/*.vhd
	for File in $Files; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		elif [ "$SKIP_LARGE_FILES" == "TRUE" ] && [ $(du -b "$File" | awk '{ print $1}') -gt $LARGE_FILESIZE ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping large '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=unisim "$File" 2>&1 | grcat $GRCRulesFile
		fi
	done
fi

# compile unisim secureip primitives
if [ "$UNISIM" == "TRUE" ] && [ "$SECUREIP" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library secureip primitives${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=synopsys --std=93c)
	Files=$SourceDir/unisims/secureip/*.vhd
	for File in $Files; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		elif [ "$SKIP_LARGE_FILES" == "TRUE" ] && [ $(du -b "$File" | awk '{ print $1}') -gt $LARGE_FILESIZE ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping large '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=secureip "$File" 2>&1 | grcat $GRCRulesFile
		fi
	done
fi

# Library unimacro
# ==============================================================================
# compile unimacro packages
if [ "$UNIMACRO" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'unimacro' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=synopsys --std=93c)
	Files=(
		$SourceDir/unimacro/unimacro_VCOMP.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing package '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=unimacro "$File" 2>&1 | grcat $GRCRulesFile
		fi
	done
fi
	
# compile unimacro macros
if [ "$UNIMACRO" == "TRUE" ]; then
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=synopsys --std=93c)
	Files=$SourceDir/unimacro/*_MACRO.vhd*
	for File in $Files; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=unimacro "$File" 2>&1 | grcat $GRCRulesFile
		fi
	done
fi

# Library simprim
# ==============================================================================
# compile simprim packages
if [ "$SIMPRIM" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'simprim' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=synopsys --std=93c)
	Files=(
		$SourceDir/simprims/simprim_Vpackage.vhd
		$SourceDir/simprims/simprim_Vcomponents.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing package '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=simprim "$File" 2>&1 | grcat $GRCRulesFile
		fi
	done
fi

# compile UNISIM primitives
if [ "$SIMPRIM" == "TRUE" ]; then
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=synopsys --std=93c)
	Files=$SourceDir/simprims/primitive/other/*.vhd*
	for File in $Files; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		elif [ "$SKIP_LARGE_FILES" == "TRUE" ] && [ $(du -b "$File" | awk '{ print $1}') -gt $LARGE_FILESIZE ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping large '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=simprim "$File" 2>&1 | grcat $GRCRulesFile
		fi
	done
fi

# compile UNISIM secureip primitives
if [ "$SIMPRIM" == "TRUE" ] && [ "$SECUREIP" == "TRUE" ]; then
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=synopsys --std=93c)
	Files=`ls -v $SourceDir/simprims/secureip/other/*.vhd*`
	for File in $Files; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping package '$File'${ANSI_RESET}"
		elif [ "$SKIP_LARGE_FILES" == "TRUE" ] && [ $(du -b "$File" | awk '{ print $1}') -gt $LARGE_FILESIZE ]; then
			echo -n ""
#			echo -e "${ANSI_CYAN}Skipping large '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing primitive '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=simprim "$File" 2>&1 | grcat $GRCRulesFile
		fi
	done
fi
	
echo "--------------------------------------------------------------------------------"
echo -n "Compiling Xilinx ISE libraries "
if [ "$STOPCOMPILING" == "TRUE" ]; then
	echo -e $COLORED_FAILED
else
	echo -e $COLORED_SUCCESSFUL
fi

cd $WorkingDir
