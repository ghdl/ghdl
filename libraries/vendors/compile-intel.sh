#! /usr/bin/env bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	Bash Script:				Script to compile the simulation libraries from Intel
#											Quartus for GHDL on Linux
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Intel Quartus Prime simulation libraries and packages
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
COMPILE_ALTERA=0
COMPILE_MAX=0
COMPILE_CYCLONE=0
COMPILE_ARRIA=0
COMPILE_STRATIX=0
COMPILE_NM=0
VERBOSE=0
DEBUG=0
FILTERING=0  # TODO: 1
SKIP_EXISTING_FILES=0
SKIP_LARGE_FILES=0
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
		--altera)
			COMMAND=3
			COMPILE_ALTERA=1
			;;
		--max)
			COMMAND=3
			COMPILE_MAX=1
			;;
		--cyclone)
			COMMAND=3
			COMPILE_CYCLONE=1
			;;
		--arria)
			COMMAND=3
			COMPILE_ARRIA=1
			;;
		--stratix)
			COMMAND=3
			COMPILE_STRATIX=1
			;;
		--nanometer)
			COMMAND=3
			COMPILE_NM=1
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
	echo "  A script to compile the Intel Quartus Prime simulation libraries for GHDL on Linux."
	echo "  One library folder 'lib/v??' per VHDL library will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	echo ""
	echo "Usage:"
	echo "  compile-intel.sh [<verbosity>] <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help                Print this help page"
	echo "  -c --clean               Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all                 Compile all Intel simulation libraries."
	echo "     --intel               Compile the Altera standard libraries: lpm, sgate, altera, altera_mf, altera_lnsim."
	echo "     --max                 Compile the Intel Max device libraries."
	echo "     --cyclone             Compile the Intel Cyclone device libraries."
	echo "     --arria               Compile the Intel Arria device libraries."
	echo "     --stratix             Compile the Intel Stratix device libraries."
	echo "     --nanometer           Unknown device library."
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
	COMPILE_ALTERA=1
	COMPILE_MAX=1
	COMPILE_CYCLONE=1
	COMPILE_ARRIA=1
	COMPILE_STRATIX=1
	COMPILE_NM=1
fi

if [[ $VHDLStandard -eq 2008 ]]; then
	echo -e "${ANSI_RED}Not all Altera packages are VHDL-2008 compatible! Setting HALT_ON_ERROR to FALSE.${ANSI_NOCOLOR}"
	HALT_ON_ERROR=0
fi

DefaultDirectories=("/opt/Altera" "/opt/altera")
if [ ! -z $QUARTUS_ROOTDIR ]; then
	EnvSourceDir=$QUARTUS_ROOTDIR/${SourceDirectories[AlteraQuartus]}
else
	for DefaultDir in ${DefaultDirectories[@]}; do
		for Major in 17 16 15 14 13; do
			for Minor in 3 2 1 0; do
				Dir=$DefaultDir/${Major}.${Minor}/quartus
				if [ -d $Dir ]; then
					EnvSourceDir=$Dir/${SourceDirectories[AlteraQuartus]}
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
# -> $DestDir
# <= $SourceDirectory
# <= $DestinationDirectory
SetupDirectories AlteraQuartus "Intel Quartus"

# create "osvvm" directory and change to it
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
	--warn-binding
	--mb-comments
)

# Create a set of GHDL parameters
GHDL_PARAMS=(${GHDL_OPTIONS[@]})
GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard -P$DestinationDirectory)

STOPCOMPILING=0

# Cleanup directories
# ==============================================================================
if [[ $CLEAN -eq 1 ]]; then
	echo 1>&2 -e "${COLORED_ERROR} '--clean' is not implemented!"
	exit 1
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_NOCOLOR}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi


# Intel standard libraries
# ==============================================================================
if [[ $COMPILE_ALTERA -eq 1 ]]; then
	LPM_VHDLVersion=$VHDLVersion
	LPM_LibraryName="lpm"
	LPM_LibraryPath="."
	LPM_Files=(
		220pack.vhd
		220model.vhd
	)
	
	SGATE_VHDLVersion=$VHDLVersion
	SGATE_LibraryName="sgate"
	SGATE_LibraryPath="."
	SGATE_Files=(
		sgate_pack.vhd
		sgate.vhd
	)
	
	ALTERA_VHDLVersion=$VHDLVersion
	ALTERA_LibraryName="altera"
	ALTERA_LibraryPath="."
	ALTERA_Files=(
		altera_europa_support_lib.vhd
		altera_primitives_components.vhd
		altera_primitives.vhd
		altera_standard_functions.vhd
		altera_syn_attributes.vhd
		alt_dspbuilder_package.vhd
	)
	
	ALTERA_MF_VHDLVersion=$VHDLVersion
	ALTERA_MF_LibraryName="altera_mf"
	ALTERA_MF_LibraryPath="."
	ALTERA_MF_Files=(
		altera_mf_components.vhd
		altera_mf.vhd
	)
	
	ALTERA_LNSIM_VHDLVersion=$VHDLVersion
	ALTERA_LNSIM_LibraryName="altera_lnsim"
	ALTERA_LNSIM_LibraryPath="."
	ALTERA_LNSIM_Files=(
		altera_lnsim_components.vhd
	)
	
	if [[ $DEBUG -eq 1 ]]; then
		for VHDLLibrary in "LPM SGATE ALTERA ALTERA_MF ALTERA_LNSIM"; do
			LibraryName="${VHDLLibrary}_LibraryName"; local LibraryName=${!LibraryName}
			Files="${VHDLLibrary}_Files[*]";          local Files=${!Files}
			
			echo -e "    ${ANSI_DARK_GRAY}VHDL Library name: $LibraryName${ANSI_NOCOLOR}"
			for File in ${Files[*]}; do
				echo -e "      ${ANSI_DARK_GRAY}$File${ANSI_NOCOLOR}"
			done
		done
	fi
fi

# Altera device libraries
# ==============================================================================
# compile Max library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_MAX" == "TRUE" ]; then
	Library="max"
	Files=(
		max_atoms.vhd
		max_components.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile MaxII library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_MAX" == "TRUE" ]; then
	Library="maxii"
	Files=(
		maxii_atoms.vhd
		maxii_components.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile MaxV library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_MAX" == "TRUE" ]; then
	Library="maxv"
	Files=(
		maxv_atoms.vhd
		maxv_components.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile ArriaII library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ARRIA" == "TRUE" ]; then
	Library="arriaii"
	Files=(
		arriaii_atoms.vhd
		arriaii_components.vhd
	)
	if [ $SKIP_LARGE_FILES -eq 0 ]; then
		Files+=(
			arriaii_hssi_components.vhd
			arriaii_hssi_atoms.vhd
		)
	fi
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ARRIA" == "TRUE" ] && [ $SKIP_LARGE_FILES -eq 0 ]; then
	Library="arriaii_pcie_hip"
	Files=(
		arriaii_pcie_hip_components.vhd
		arriaii_pcie_hip_atoms.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile ArriaIIGZ library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ARRIA" == "TRUE" ]; then
	Library="arriaiigz"
	Files=(
		arriaiigz_atoms.vhd
		arriaiigz_components.vhd
	)
	if [ $SKIP_LARGE_FILES -eq 0 ]; then
		Files+=(
			arriaiigz_hssi_components.vhd
		)
	fi
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile ArriaV library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ARRIA" == "TRUE" ]; then
	Library="arriav"
	Files=(
		arriav_atoms.vhd
		arriav_components.vhd
	)
	if [ $SKIP_LARGE_FILES -eq 0 ]; then
		Files+=(
			arriav_hssi_components.vhd
			arriav_hssi_atoms.vhd
		)
	fi
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile ArriaVGZ library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ARRIA" == "TRUE" ]; then
	Library="arriavgz"
	Files=(
		arriavgz_atoms.vhd
		arriavgz_components.vhd
	)
	if [ $SKIP_LARGE_FILES -eq 0 ]; then
		Files+=(
			arriavgz_hssi_components.vhd
			arriavgz_hssi_atoms.vhd
		)
	fi
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ARRIA" == "TRUE" ] && [ $SKIP_LARGE_FILES -eq 0 ]; then
	Library="arriavgz_pcie_hip"
	Files=(
		arriavgz_pcie_hip_components.vhd
		arriavgz_pcie_hip_atoms.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile CycloneIV library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_CYCLONE" == "TRUE" ]; then
	Library="cycloneiv"
	Files=(
		cycloneiv_atoms.vhd
		cycloneiv_components.vhd
	)
	if [ $SKIP_LARGE_FILES -eq 0 ]; then
		Files+=(
			cycloneiv_hssi_components.vhd
			cycloneiv_hssi_atoms.vhd
		)
	fi
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_CYCLONE" == "TRUE" ] && [ $SKIP_LARGE_FILES -eq 0 ]; then
	Library="cycloneiv_pcie_hip"
	Files=(
		cycloneiv_pcie_hip_components.vhd
		cycloneiv_pcie_hip_atoms.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile CycloneIVE library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_CYCLONE" == "TRUE" ]; then
	Library="cycloneive"
	Files=(
		cycloneive_atoms.vhd
		cycloneive_components.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile CycloneV library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_CYCLONE" == "TRUE" ]; then
	Library="cyclonev"
	Files=(
		cyclonev_atoms.vhd
		cyclonev_components.vhd
	)
	if [ $SKIP_LARGE_FILES -eq 0 ]; then
		Files+=(
			cyclonev_hssi_components.vhd
			cyclonev_hssi_atoms.vhd
		)
	fi
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile StratixIV library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_STRATIX" == "TRUE" ]; then
	Library="stratixiv"
	Files=(
		stratixiv_atoms.vhd
		stratixiv_components.vhd
	)
	if [ $SKIP_LARGE_FILES -eq 0 ]; then
		Files+=(
			stratixiv_hssi_components.vhd
			stratixiv_hssi_atoms.vhd
		)
	fi
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_STRATIX" == "TRUE" ] && [ $SKIP_LARGE_FILES -eq 0 ]; then
	Library="stratixiv_pcie_hip"
	Files=(
		stratixiv_pcie_hip_components.vhd
		stratixiv_pcie_hip_atoms.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile StratixV library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_STRATIX" == "TRUE" ]; then
	Library="stratixv"
	Files=(
		stratixv_atoms.vhd
		stratixv_components.vhd
	)
	if [ $SKIP_LARGE_FILES -eq 0 ]; then
		Files+=(
			stratixv_hssi_components.vhd
			stratixv_hssi_atoms.vhd
		)
	fi
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_STRATIX" == "TRUE" ] && [ $SKIP_LARGE_FILES -eq 0 ]; then
	Library="stratixv_pcie_hip"
	Files=(
		stratixv_pcie_hip_components.vhd
		stratixv_pcie_hip_atoms.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile fiftyfivenm library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_NM" == "TRUE" ]; then
	Library="fiftyfivenm"
	Files=(
		fiftyfivenm_atoms.vhd
		fiftyfivenm_components.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile twentynm library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_NM" == "TRUE" ]; then
	Library="twentynm"
	Files=(
		twentynm_atoms.vhd
		twentynm_components.vhd
	)
	if [ $SKIP_LARGE_FILES -eq 0 ]; then
		Files+=(
			twentynm_hip_components.vhd
			twentynm_hip_atoms.vhd
			twentynm_hssi_components.vhd
			twentynm_hssi_atoms.vhd
		)
	fi
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

if [[ $Libraries != "" ]]; then
	Compile "$SourceDirectory" "$Libraries"
	
	echo "--------------------------------------------------------------------------------"
	echo -e "Compiling Intel Quartus packages and device libraries $(test $ERRORCOUNT -eq 0 && echo $COLORED_SUCCESSFUL || echo $COLORED_FAILED)"
else
	echo -e "${ANSI_RED}Neither Intel Quartus packages nor device libraries selected.${ANSI_NOCOLOR}"
fi
