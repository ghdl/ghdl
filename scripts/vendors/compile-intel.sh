#! /usr/bin/env bash
# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
#  Bash Script (executable):
#    Script to compile the simulation libraries from Altera Quartus for GHDL on
#    Linux
#
# Description:
#    - Creates a subdirectory in the current working directory
#    - Compiles all Altera Quartus-II simulation libraries and packages
#
# ==============================================================================
#  Copyright (C) 2017-2025 Patrick Lehmann - Boetzingen, Germany
#  Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
# ==============================================================================

# Work around for Darwin (Mac OS)
test greadlink --version > /dev/null 2>&1 && READLINK=greadlink || READLINK=readlink

# Save working directory
WorkingDir=$(pwd)
ScriptDir="$(dirname $0)"
ScriptDir="$($READLINK -f $ScriptDir)"

# Source Bash utilities
source $ScriptDir/../ansi_color.sh
if [[ $? -ne 0 ]]; then
	printf "\x1b[31m[ERROR] %s\x1b[0m\n" "While loading Bash utilities." 1>&2
	exit 1
fi


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
FILTERING=1
SKIP_LARGE_FILES=0
SUPPRESS_WARNINGS=0
HALT_ON_ERROR=0
VHDLStandard=93
DestDir=""
SrcDir=""
while [[ $# -gt 0 ]]; do
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
		--src|--source)
			SrcDir="$2"
			shift						# skip argument
			;;
		--out|--output)
			DestDir="$2"
			shift						# skip argument
			;;
		*)		# unknown option
			PrintError "Unknown command line option '$1'."
			COMMAND=0
			break
			;;
	esac
	shift # parsed argument or value
done

ERRORCOUNT=0
Libraries=()

if [[ $COMMAND -le 1 ]]; then
	test $COMMAND -eq 1 && PrintError "No command selected."
	printf "\n"
	printf "%s\n" "Synopsis:"
	printf "%s\n" "  A script to compile the Intel Quartus Prime simulation libraries for GHDL on Linux."
	printf "%s\n" "  One library folder 'lib/v??' per VHDL library will be created relative to the current"
	printf "%s\n" "  working directory."
	printf "\n"
	printf "%s\n" "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	printf "\n"
	printf "%s\n" "Usage:"
	printf "%s\n" "  $(basename "$0") [<verbosity>] <common command>|<library> [<options>] [<adv. options>]"
	printf "\n"
	printf "%s\n" "Common commands:"
	printf "%s\n" "  -h --help                    Print this help page"
	printf "%s\n" "  -c --clean                   Remove all generated files"
	printf "\n"
	printf "%s\n" "Libraries:"
	printf "%s\n" "  -a --all                     Compile all Intel simulation libraries."
	printf "%s\n" "     --intel                   Compile the Altera standard libraries: lpm, sgate, altera, altera_mf, altera_lnsim."
	printf "%s\n" "     --max                     Compile the Intel Max device libraries."
	printf "%s\n" "     --cyclone                 Compile the Intel Cyclone device libraries."
	printf "%s\n" "     --arria                   Compile the Intel Arria device libraries."
	printf "%s\n" "     --stratix                 Compile the Intel Stratix device libraries."
	printf "%s\n" "     --nanometer               Unknown device library."
	printf "\n"
	printf "%s\n" "Library compile options:"
	printf "%s\n" "     --vhdl93                  Compile the libraries with VHDL-93."
	printf "%s\n" "     --vhdl2008                Compile the libraries with VHDL-2008."
	printf "%s\n" "  -S --skip-largefiles         Don't compile large files. Exclude *HSSI* and *HIP* files."
	printf "%s\n" "  -H --halt-on-error           Halt on error(s)."
	printf "\n"
	printf "%s\n" "Advanced options:"
	printf "%s\n" "  --ghdl <GHDL binary>         Path to GHDL's executable, e.g. /usr/local/bin/ghdl"
	printf "%s\n" "  --output <dir name>          Name of the output directory, e.g. intel"
	printf "%s\n" "  --source <Path to Quartus>   Path to the sources."
	printf "\n"
	printf "%s\n" "Verbosity:"
	printf "%s\n" "  -v --verbose                 Print verbose messages."
	printf "%s\n" "  -d --debug                   Print debug messages."
	printf "%s\n" "  -n --no-filter               Disable output filtering scripts."
	printf "%s\n" "  -N --no-warnings             Suppress all warnings. Show only error messages."
	printf "\n"
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


# Source configuration file from GHDL's 'vendors' library directory
Chapter "Loading environment..."
source $ScriptDir/config.sh
CheckError $? "While loading configuration."

source $ScriptDir/shared.sh
CheckError $? "While loading further procedures."

# Warn that some files might not be VHDL-2008 ready. Thus enabled continue on error.
if [[ $VHDLStandard -eq 2008 ]]; then
	PrintWarning "${ANSI_RED}Not all Altera packages are VHDL-2008 compatible! Setting ${ANSI_LIGHT_RED}CONTINUE_ON_ERROR${ANSI_RED} to ${ANSI_LIGHT_RED}TRUE${ANSI_RED}."
	CONTINUE_ON_ERROR=1
fi

# Search Intel Quartus in default installation locations
DefaultDirectories=("/opt/IntelFPGA" "/opt/intelfpga" "/opt/Intel" "/opt/intel" "/opt/Altera" "/opt/altera" "/c/intelFPGA")
if [ ! -z $QUARTUS_ROOTDIR ]; then
	EnvSourceDir="$QUARTUS_ROOTDIR/${Intel_Quartus_Settings[SourceDirectory]}"
else
	for DefaultDir in "${DefaultDirectories[@]}"; do
		for Major in 21 20 19 18 17 16; do
			for Minor in 4 3 2 1 0; do
				Dir=$DefaultDir/${Major}.${Minor}/quartus
				if [ -d $Dir ]; then
					EnvSourceDir="$Dir/${Intel_Quartus_Settings[SourceDirectory]}"
					break 3
				fi
			done
		done
	done
fi


# <= $VHDLVersion
# <= $VHDLStandard
# <= $VHDLFlavor
GHDLSetup $VHDLStandard

# -> $SourceDirectories
# -> $DestinationDirectories
# -> $SrcDir
# -> $DestDir
# <= $SourceDirectory
# <= $DestinationDirectory
SetupDirectories Intel_Quartus "Intel Quartus"

# create "osvvm" directory and change to it
# => $DestinationDirectory
CreateDestinationDirectory
cd $DestinationDirectory


# Extend global GHDL Options TODO: move to GHDLSetup
Analyze_Parameters+=(
	-fexplicit
	-Wbinding
)
if [[ $DEBUG -eq 0 ]]; then
	Analyze_Parameters+=(
		-Wno-hide
	)
fi
if [[ ! (VERBOSE -eq 1) && ($DEBUG -eq 1) ]]; then
	Analyze_Parameters+=(
		-Wno-others
		-Wno-static
	)
fi
Analyze_Parameters+=(
	--ieee=$VHDLFlavor
	--no-vital-checks
	--std=$VHDLStandard
	-frelaxed
	-P$DestinationDirectory
)

# Cleanup directories
# ==============================================================================
if [[ $CLEAN -eq 1 ]]; then
	Chapter "Cleaning up vendor directory ..."
	if [[ $COMPILE_ALTERA -eq 1 && -d "altera" ]]; then
		PrintVerbose "Deleting 'altera'"
		for Device in altera altera_mf altera_lnsim sgate lpm; do
			PrintDebug "rm -rf "${Device}" 2> /dev/null"
			rm -rf "${Device}" 2> /dev/null
		done
	fi
	if [[ $COMPILE_MAX -eq 1 && -d "max" ]]; then
		PrintVerbose "Deleting 'max'"
		for Device in max maxii maxv; do
			PrintDebug "rm -rf "${Device}" 2> /dev/null"
			rm -rf "${Device}" 2> /dev/null
		done
	fi
	if [[ $COMPILE_CYCLONE -eq 1 && -d "cyclone" ]]; then
		PrintVerbose "Deleting 'cyclone'"
		for Device in cycloneiv cycloneiv_pcie_hip cyclonev cycloneive cycloneiv; do
			PrintDebug "rm -rf "${Device}" 2> /dev/null"
			rm -rf "${Device}" 2> /dev/null
		done
	fi
	if [[ $COMPILE_ARRIA -eq 1 && -d "arria" ]]; then
		PrintVerbose "Deleting 'arria'"
		for Device in arriaii arriaii_pcie_hip; do
			PrintDebug "rm -rf "${Device}" 2> /dev/null"
			rm -rf "${Device}" 2> /dev/null
		done
	fi
#	if [[ $COMPILE_STRATIX -eq 1 && -d "stratix" ]]; then
#		PrintVerbose "Deleting 'stratix'"
#		for Device in altera altera_mf altera_lnsim; do
#			PrintDebug "rm -rf "${Device}" 2> /dev/null"
#			rm -rf "${Device}" 2> /dev/null
#		done
#	fi
	if [[ $COMPILE_NM -eq 1 && -d "nm" ]]; then
		PrintVerbose "Deleting 'nm'"
		for Device in fiftyfivenm; do
			PrintDebug "rm -rf "${Device}" 2> /dev/null"
			rm -rf "${Device}" 2> /dev/null
		done
	fi
fi


# Intel standard libraries
# ==============================================================================
StructName="LPM"
Files=(
	220pack.vhd
	220model.vhd
)
CreateLibraryStruct $StructName "lpm" "." $VHDLVersion "${Files[@]}"
test $COMPILE_ALTERA -eq 1 && Libraries+=("$StructName")

StructName="SGATE"
Files=(
	sgate_pack.vhd
	sgate.vhd
)
CreateLibraryStruct $StructName "sgate" "." $VHDLVersion "${Files[@]}"
test $COMPILE_ALTERA -eq 1 && Libraries+=("$StructName")

StructName="ALTERA"
Files=(
	altera_europa_support_lib.vhd
	altera_primitives_components.vhd
	altera_primitives.vhd
	altera_standard_functions.vhd
	altera_syn_attributes.vhd
	alt_dspbuilder_package.vhd
)
CreateLibraryStruct $StructName "altera" "." $VHDLVersion "${Files[@]}"
test $COMPILE_ALTERA -eq 1 && Libraries+=("$StructName")

StructName="ALTERA_MF"
Files=(
	altera_mf_components.vhd
	altera_mf.vhd
)
CreateLibraryStruct $StructName "altera_mf" "." $VHDLVersion "${Files[@]}"
test $COMPILE_ALTERA -eq 1 && Libraries+=("$StructName")


StructName="ALTERA_LNSIM"
Files=(
	altera_lnsim_components.vhd
)
CreateLibraryStruct $StructName "altera_lnsim" "." $VHDLVersion "${Files[@]}"
test $COMPILE_ALTERA -eq 1 && Libraries+=("$StructName")

# Intel device libraries
# ==============================================================================
Chapter "Searching available devices ..."

# Max library
StructName="MAX"
Files=(
	max_atoms.vhd
	max_components.vhd
)
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Max'."
	CreateLibraryStruct $StructName "max" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_MAX -eq 1 && Libraries+=("$StructName")
fi

# Max II library
StructName="MAX_II"
Files=(
	maxii_atoms.vhd
	maxii_components.vhd
)
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Max II'."
	CreateLibraryStruct $StructName "maxii" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_MAX -eq 1 && Libraries+=("$StructName")
fi

# Max V library
StructName="MAX_V"
Files=(
	maxv_atoms.vhd
	maxv_components.vhd
)
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Max V'."
	CreateLibraryStruct $StructName "maxv" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_MAX -eq 1 && Libraries+=("$StructName")
fi

# Arria II library
StructName="ARRIA_II"
Files=(
	arriaii_atoms.vhd
	arriaii_components.vhd
)
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	Files+=(
		arriaii_hssi_components.vhd
		arriaii_hssi_atoms.vhd
	)
fi
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Arria II'."
	CreateLibraryStruct $StructName "arriaii" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_ARRIA -eq 1 && Libraries+=("$StructName")
fi

# Arria II (PCIe) library
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	StructName="ARRIA_II_PCIe"
	Files=(
		arriaii_pcie_hip_components.vhd
		arriaii_pcie_hip_atoms.vhd
	)
	if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
		PrintDebug "Found device 'Arria II (PCIe)'."
		CreateLibraryStruct $StructName "arriaii_pcie_hip" "." $VHDLVersion "${Files[@]}"

		test $COMPILE_ARRIA -eq 1 && Libraries+=("$StructName")
	fi
fi

# ArriaII GZ library
StructName="ARRIA_II_GZ"
Files=(
	arriaiigz_atoms.vhd
	arriaiigz_components.vhd
)
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	Files+=(
		arriaiigz_hssi_components.vhd
	)
fi
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Arria II GZ'."
	CreateLibraryStruct $StructName "arriaiigz" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_ARRIA -eq 1 && Libraries+=("$StructName")
fi

# ArriaV library
StructName="ARRIA_V"
Files=(
	arriav_atoms.vhd
	arriav_components.vhd
)
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	Files+=(
		arriav_hssi_components.vhd
		arriav_hssi_atoms.vhd
	)
fi
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Arria V'."
	CreateLibraryStruct $StructName "arriav" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_ARRIA -eq 1 && Libraries+=("$StructName")
fi

# Arria V GZ library
StructName="ARRIA_V_GZ"
Files=(
	arriavgz_atoms.vhd
	arriavgz_components.vhd
)
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	Files+=(
		arriavgz_hssi_components.vhd
		arriavgz_hssi_atoms.vhd
	)
fi
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Arria V GZ'."
	CreateLibraryStruct $StructName "arriavgz" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_ARRIA -eq 1 && Libraries+=("$StructName")
fi

# Arria V GZ (PCIe) library
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	StructName="ARRIA_V_GZ_PCIe"
	Files=(
		arriavgz_pcie_hip_components.vhd
		arriavgz_pcie_hip_atoms.vhd
	)
	if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
		PrintDebug "Found device 'Arria V GZ (PCIe)'."
		CreateLibraryStruct $StructName "arriavgz_pcie_hip" "." $VHDLVersion "${Files[@]}"

		test $COMPILE_ARRIA -eq 1 && Libraries+=("$StructName")
	fi
fi

# Cyclone IV library
StructName="CYCLONE_IV"
Files=(
	cycloneiv_atoms.vhd
	cycloneiv_components.vhd
)
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	Files+=(
		cycloneiv_hssi_components.vhd
		cycloneiv_hssi_atoms.vhd
	)
fi
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Cyclone IV'."
	CreateLibraryStruct $StructName "cycloneiv" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_CYCLONE -eq 1 && Libraries+=("$StructName")
fi

# Cyclone IV (PCIe) library
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	StructName="CYCLONE_IV_PCIe"
	Files=(
		cycloneiv_pcie_hip_components.vhd
		cycloneiv_pcie_hip_atoms.vhd
	)
	if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
		PrintDebug "Found device 'Cyclone IV (PCIe)'."
		CreateLibraryStruct $StructName "cycloneiv_pcie_hip" "." $VHDLVersion "${Files[@]}"

		test $COMPILE_CYCLONE -eq 1 && Libraries+=("$StructName")
	fi
fi

# Cyclone IV E library
StructName="CYCLONE_IV_E"
Files=(
	cycloneive_atoms.vhd
	cycloneive_components.vhd
)
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Cyclone IV E'."
	CreateLibraryStruct $StructName "cycloneive" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_CYCLONE -eq 1 && Libraries+=("$StructName")
fi

# Cyclone V library
StructName="CYCLONE_V"
Files=(
	cyclonev_atoms.vhd
	cyclonev_components.vhd
)
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	Files+=(
		cyclonev_hssi_components.vhd
		cyclonev_hssi_atoms.vhd
	)
fi
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Cyclone V'."
	CreateLibraryStruct $StructName "cyclonev" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_CYCLONE -eq 1 && Libraries+=("$StructName")
fi

# Stratix IV library
StructName="STRATIX_IV"
Files=(
	stratixiv_atoms.vhd
	stratixiv_components.vhd
)
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	Files+=(
		stratixiv_hssi_components.vhd
		stratixiv_hssi_atoms.vhd
	)
fi
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Stratix IV'."
	CreateLibraryStruct $StructName "stratixiv" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_STRATIX -eq 1 && Libraries+=("$StructName")
fi

# Stratix IV (PCIe) library
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	StructName="STRATIX_IV_PCIe"
	Files=(
		stratixiv_pcie_hip_components.vhd
		stratixiv_pcie_hip_atoms.vhd
	)
	if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
		PrintDebug "Found device 'Stratix IV (PCIe)'."
		CreateLibraryStruct $StructName "stratixiv_pcie_hip" "." $VHDLVersion "${Files[@]}"

		test $COMPILE_STRATIX -eq 1 && Libraries+=("$StructName")
	fi
fi

# Stratix V library
StructName="STRATIX_V"
Files=(
	stratixv_atoms.vhd
	stratixv_components.vhd
)
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	Files+=(
		stratixv_hssi_components.vhd
		stratixv_hssi_atoms.vhd
	)
fi
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device 'Stratix V'."
	CreateLibraryStruct $StructName "stratixv" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_STRATIX -eq 1 && Libraries+=("$StructName")
fi

# Stratix V (PCIe) library
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	StructName="STRATIX_V_PCIe"
	Files=(
		stratixv_pcie_hip_components.vhd
		stratixv_pcie_hip_atoms.vhd
	)
	if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
		PrintDebug "Found device 'Stratix V (PCIe)'."
		CreateLibraryStruct $StructName "stratixv_pcie_hip" "." $VHDLVersion "${Files[@]}"

		test $COMPILE_STRATIX -eq 1 && Libraries+=("$StructName")
	fi
fi

# 55 nm library
StructName="NM_55"
Files=(
	fiftyfivenm_atoms.vhd
	fiftyfivenm_components.vhd
)
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device '55 nm'."
	CreateLibraryStruct $StructName "fiftyfivenm" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_NM -eq 1 && Libraries+=("$StructName")
fi

# 20 nm library
StructName="NM_20"
Files=(
	twentynm_atoms.vhd
	twentynm_components.vhd
)
if [[ $SKIP_LARGE_FILES -eq 0 ]]; then
	Files+=(
		twentynm_hip_components.vhd
		twentynm_hip_atoms.vhd
		twentynm_hssi_components.vhd
		twentynm_hssi_atoms.vhd
	)
fi
if [[ -f "$SourceDirectory/${Files[0]}" ]]; then
	PrintDebug "Found device '20 nm'."
	CreateLibraryStruct $StructName "twentynm" "." $VHDLVersion "${Files[@]}"

	test $COMPILE_NM -eq 1 && Libraries+=("$StructName")
fi

# if [[ $DEBUG -eq 1 ]]; then
	# for StructName in ${Libraries[*]}; do
		# PrintLibraryStruct $StructName "    "
	# done
# fi

if [[ ${#Libraries[@]} -ne 0 ]]; then
	Compile "$SourceDirectory" "${Libraries[*]}"

	printf "%s\n" "--------------------------------------------------------------------------------"
	printf "Compiling Intel Quartus packages and device libraries %s\n" "$(test $ERRORCOUNT -eq 0 && echo $COLORED_SUCCESSFUL || echo $COLORED_FAILED)"
else
	PrintErrorAndExit "Neither Intel Quartus packages nor device libraries selected." 2
fi
