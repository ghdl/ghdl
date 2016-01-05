#! /bin/bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Bash Script:				Script to compile the simulation libraries from Altera
#											Quartus-II for GHDL on Linux
# 
#	Authors:						Patrick Lehmann
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Altera Quartus-II simulation libraries and packages
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
		--altera)
		ALTERA=TRUE
		NO_COMMAND=FALSE
		;;
		--max)
		MAX=TRUE
		NO_COMMAND=FALSE
		;;
		--cyclone)
		CYCLONE=TRUE
		NO_COMMAND=FALSE
		;;
		--arria)
		ARRIA=TRUE
		NO_COMMAND=FALSE
		;;
		--stratix)
		STRATIX=TRUE
		NO_COMMAND=FALSE
		;;
		--nanometer)
		NANOMETER=TRUE
		NO_COMMAND=FALSE
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
	echo "  Script to compile the simulation libraries from Altera Quartus-II for GHDL on Linux"
	echo ""
	echo "Usage:"
	echo "  compile-altera.sh <common command>|<library> [<options>]"
#         [-v] [-c] [--unisim] [--unimacro] [--simprim] [--secureip] [-s|--skip-existing] [-S|--skip-largefiles] [-n|--no-warnings]
	echo ""
	echo "Common commands:"
	echo "  -h --help             Print this help page"
	echo "  -c --clean            Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all              Compile all Altera simulation libraries."
	echo "     --altera           Compile the Altera standard libraries: lpm, sgate, altera, altera_mf, altera_lnsim."
	echo "     --max              Compile the Altera Max device libraries."
	echo "     --cyclone          Compile the Altera Cyclone device libraries."
	echo "     --arria            Compile the Altera Arria device libraries."
	echo "     --stratix          Compile the Altera Stratix device libraries."
	echo "     --nanometer        Unknown device library."
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
	ALTERA=TRUE
	MAX=TRUE
	CYCLONE=TRUE
	ARRIA=TRUE
	STRATIX=TRUE
	NANOMETER=TRUE
fi

if [ "$VHDL93" == "TRUE" ]; then
	VHDLStandard="93c"
	VHDLFlavor="synopsys"
elif [ "$VHDL2008" == "TRUE" ]; then
	VHDLStandard="08"
	VHDLFlavor="standard"
else
	VHDLStandard="93c"
	VHDLFlavor="synopsys"
fi

# extract data from configuration
InstallDir=${InstallationDirectory[AlteraQuartusII]}
SourceDir="$InstallDir/quartus/eda/sim_lib"
DestinationDir=${DestinationDirectory[AlteraQuartusII]}

if [ -z $InstallDir ] || [ -z $DestinationDir ]; then
	echo -e "${COLORED_ERROR} Altera Quartus-II is not configured in '$ScriptDir/config.sh'${ANSI_RESET}"
	exit -1
elif [ ! -d $SourceDir ]; then
	echo -e "${COLORED_ERROR} Path '$SourceDir' does not exist.${ANSI_RESET}"
	exit -1
fi

# set bash options
set -o pipefail

# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)

# create "altera" directory and change to it
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

# Altera standard libraries
# ==============================================================================
# compile lpm library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ALTERA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'lpm' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/220pack.vhd
		$SourceDir/220model.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=lpm "$File" 2>&1 | $GRC_COMMAND
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

# compile sgate library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ALTERA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'sgate' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/sgate_pack.vhd
		$SourceDir/sgate.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=sgate "$File" 2>&1 | $GRC_COMMAND
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

# compile altera library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ALTERA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'altera' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/altera_europa_support_lib.vhd
		$SourceDir/altera_primitives_components.vhd
		$SourceDir/altera_primitives.vhd
		$SourceDir/altera_standard_functions.vhd
		$SourceDir/altera_syn_attributes.vhd
		$SourceDir/alt_dspbuilder_package.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=altera "$File" 2>&1 | $GRC_COMMAND
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

# compile altera_mf library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ALTERA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'altera_mf' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/altera_mf_components.vhd
		$SourceDir/altera_mf.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=altera_mf "$File" 2>&1 | $GRC_COMMAND
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

# compile altera_lnsim library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ALTERA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'altera_lnsim' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/altera_lnsim_components.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=altera_lnsim "$File" 2>&1 | $GRC_COMMAND
			if [ $? -ne 0 ] && [ "$HALT_ON_ERROR" == "TRUE" ]; then
				STOPCOMPILING=TRUE
				break
			fi
		fi
	done
fi

# Altera device libraries
# ==============================================================================
# compile Max library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$MAX" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'max' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/max_atoms.vhd
		$SourceDir/max_components.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=max "$File" 2>&1 | $GRC_COMMAND
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

# compile MaxII library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$MAX" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'maxii' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/maxii_atoms.vhd
		$SourceDir/maxii_components.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=maxii "$File" 2>&1 | $GRC_COMMAND
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

# compile MaxV library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$MAX" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'maxv' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/maxv_atoms.vhd
		$SourceDir/maxv_components.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=maxv "$File" 2>&1 | $GRC_COMMAND
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

# compile ArriaII library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ARRIA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'arriaii' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/arriaii_atoms.vhd
		$SourceDir/arriaii_components.vhd
		$SourceDir/arriaii_hssi_components.vhd
		$SourceDir/arriaii_hssi_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=arriaii "$File" 2>&1 | $GRC_COMMAND
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

if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ARRIA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'arriaii_pcie_hip' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/arriaii_pcie_hip_components.vhd
		$SourceDir/arriaii_pcie_hip_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=arriaii_pcie_hip "$File" 2>&1 | $GRC_COMMAND
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

# compile ArriaIIGZ library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ARRIA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'arriaiigz' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/arriaiigz_atoms.vhd
		$SourceDir/arriaiigz_components.vhd
		$SourceDir/arriaiigz_hssi_components.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=arriaiigz "$File" 2>&1 | $GRC_COMMAND
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

# compile ArriaV library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ARRIA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'arriav' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/arriav_atoms.vhd
		$SourceDir/arriav_components.vhd
		$SourceDir/arriav_hssi_components.vhd
		$SourceDir/arriav_hssi_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=arriav "$File" 2>&1 | $GRC_COMMAND
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

# compile ArriaVGZ library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ARRIA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'arriavgz' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/arriavgz_atoms.vhd
		$SourceDir/arriavgz_components.vhd
		$SourceDir/arriavgz_hssi_components.vhd
		$SourceDir/arriavgz_hssi_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=arriavgz "$File" 2>&1 | $GRC_COMMAND
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

if [ "$STOPCOMPILING" == "FALSE" ] && [ "$ARRIA" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'arriavgz_pcie_hip' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/arriavgz_pcie_hip_components.vhd
		$SourceDir/arriavgz_pcie_hip_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=arriavgz_pcie_hip "$File" 2>&1 | $GRC_COMMAND
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

# compile CycloneIV library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$CYCLONE" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'cycloneiv' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/cycloneiv_atoms.vhd
		$SourceDir/cycloneiv_components.vhd
		$SourceDir/cycloneiv_hssi_components.vhd
		$SourceDir/cycloneiv_hssi_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=cycloneiv "$File" 2>&1 | $GRC_COMMAND
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

if [ "$STOPCOMPILING" == "FALSE" ] && [ "$CYCLONE" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'cycloneiv_pcie_hip' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/cycloneiv_pcie_hip_components.vhd
		$SourceDir/cycloneiv_pcie_hip_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=cycloneiv_pcie_hip "$File" 2>&1 | $GRC_COMMAND
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

# compile CycloneIVE library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$CYCLONE" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'cycloneive' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/cycloneive_atoms.vhd
		$SourceDir/cycloneive_components.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=cycloneive "$File" 2>&1 | $GRC_COMMAND
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

# compile CycloneV library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$CYCLONE" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'cyclonev' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/cyclonev_atoms.vhd
		$SourceDir/cyclonev_components.vhd
		$SourceDir/cyclonev_hssi_components.vhd
		$SourceDir/cyclonev_hssi_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=cyclonev "$File" 2>&1 | $GRC_COMMAND
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

# compile StratixIV library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$STRATIX" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'stratixiv' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/stratixiv_atoms.vhd
		$SourceDir/stratixiv_components.vhd
		$SourceDir/stratixiv_hssi_components.vhd
		$SourceDir/stratixiv_hssi_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=stratixiv "$File" 2>&1 | $GRC_COMMAND
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

if [ "$STOPCOMPILING" == "FALSE" ] && [ "$STRATIX" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'stratixiv_pcie_hip' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/stratixiv_pcie_hip_components.vhd
		$SourceDir/stratixiv_pcie_hip_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=stratixiv_pcie_hip "$File" 2>&1 | $GRC_COMMAND
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

# compile StratixV library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$STRATIX" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'stratixv' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/stratixv_atoms.vhd
		$SourceDir/stratixv_components.vhd
		$SourceDir/stratixv_hssi_components.vhd
		$SourceDir/stratixv_hssi_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=stratixv "$File" 2>&1 | $GRC_COMMAND
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

if [ "$STOPCOMPILING" == "FALSE" ] && [ "$STRATIX" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'stratixv_pcie_hip' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/stratixv_pcie_hip_components.vhd
		$SourceDir/stratixv_pcie_hip_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=stratixv_pcie_hip "$File" 2>&1 | $GRC_COMMAND
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

# compile fiftyfivenm library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$NANOMETER" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'fiftyfivenm' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/fiftyfivenm_atoms.vhd
		$SourceDir/fiftyfivenm_components.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=fiftyfivenm "$File" 2>&1 | $GRC_COMMAND
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

# compile twentynm library
if [ "$STOPCOMPILING" == "FALSE" ] && [ "$NANOMETER" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Compiling library 'twentynm' ...${ANSI_RESET}"
	GHDL_PARAMS=(${GHDL_OPTIONS[@]})
	GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard)
	Files=(
		$SourceDir/twentynm_atoms.vhd
		$SourceDir/twentynm_components.vhd
		$SourceDir/twentynm_hip_components.vhd
		$SourceDir/twentynm_hip_atoms.vhd
		$SourceDir/twentynm_hssi_components.vhd
		$SourceDir/twentynm_hssi_atoms.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [ "$SKIP_EXISTING_FILES" == "TRUE" ] && [ -e "${FileName%.*}.o" ]; then
			echo -e "${ANSI_CYAN}Skipping file '$File'${ANSI_RESET}"
		else
			echo -e "${ANSI_CYAN}Analyzing file '$File'${ANSI_RESET}"
			ghdl -a ${GHDL_PARAMS[@]} --work=twentynm "$File" 2>&1 | $GRC_COMMAND
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
echo -n "Compiling Altera Quartus-II libraries "
if [ $ERRORCOUNT -gt 0 ]; then
	echo -e $COLORED_FAILED
else
	echo -e $COLORED_SUCCESSFUL
fi

cd $WorkingDir
