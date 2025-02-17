#! /usr/bin/env bash
# ==============================================================================
#  Authors:
#    Markus Koch
#    Patrick Lehmann
#
#  Bash Script (executable):
#    Script to compile the simulation libraries from Lattice Diamond for GHDL on
#    Linux
#
# Description:
#    - Creates a subdirectory in the current working directory
#    - Compiles all Lattice Diamond simulation libraries and packages
#
# ==============================================================================
#  Copyright (C) 2017-2025 Patrick Lehmann - Boetzingen, Germany
#  Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
#  Copyright (C) 2015-2016 Markus Koch
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


DeviceList="EC ECP ECP2 ECP3 ECP5U LPTM LPTM2 MACHXO MACHXO2 MACHXO3L SC SCM XP XP2"
for Device in $DeviceList; do
	declare "DEV_${Device}_Enable"=0
done


# Command line argument processing
COMMAND=1
CLEAN=0
VERBOSE=0
DEBUG=0
FILTERING=1
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
			FOUND=0
			if [[ "${1:0:2}" == "--" ]]; then
				key=${1:2}; key=${key,,}
				for Device in $DeviceList; do
					if [[ $key == "${Device,,}" ]]; then
						declare "DEV_${Device}_Enable"=1
						COMMAND=3
						FOUND=1
						break
					fi
				done
			fi
			if [[ $FOUND -eq 0 ]]; then
				PrintError "Unknown command line option '$1'."
				COMMAND=0
				break
			fi
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
	printf "%s\n" "  A script to compile the Lattice Diamond simulation libraries for GHDL on Linux."
	printf "%s\n" "  One library folder 'lib/v??' per VHDL library will be created relative to the current"
	printf "%s\n" "  working directory."
	printf "\n"
	printf "%s\n" "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	printf "\n"
	printf "%s\n" "Usage:"
	printf "%s\n" "  $(basename "$0") <common command>|<library> [<options>] [<adv. options>]"
	printf "\n"
	printf "%s\n" "Common commands:"
	printf "%s\n" "  -h --help                    Print this help page"
	printf "%s\n" "  -c --clean                   Remove all generated files"
	printf "\n"
	printf "%s\n" "Libraries:"
	printf "%s\n" "  -a --all                     Compile all Lattice simulation libraries."
	for Device in $DeviceList; do
	  printf "     --%-23s Device primitives for '%s'.\n" "${Device,,}" "$Device"
	done
	printf "\n"
	printf "%s\n" "Library compile options:"
	printf "%s\n" "     --vhdl93                  Compile the libraries with VHDL-93."
	printf "%s\n" "     --vhdl2008                Compile the libraries with VHDL-2008."
	printf "%s\n" "  -H --halt-on-error           Halt on error(s)."
	printf "\n"
	printf "%s\n" "Advanced options:"
	printf "%s\n" "  --ghdl <GHDL binary>         Path to GHDL's executable, e.g. /usr/local/bin/ghdl"
	printf "%s\n" "  --output <dir name>          Name of the output directory, e.g. lattice"
	printf "%s\n" "  --source <Path to Diamond>   Path to the sources."
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
	for Device in $DeviceList; do
		declare "DEV_${Device}_Enable"=1
	done
fi


# Source configuration file from GHDL's 'vendors' library directory
Chapter "Loading environment..."
source $ScriptDir/config.sh
CheckError $? "While loading configuration."

source $ScriptDir/shared.sh
CheckError $? "While loading further procedures."

# Warn that some files might not be VHDL-2008 ready. Thus enabled continue on error.
if [[ $VHDLStandard -eq 2008 ]]; then
	PrintWarning "${ANSI_RED}Not all Lattice packages are VHDL-2008 compatible! Setting ${ANSI_LIGHT_RED}CONTINUE_ON_ERROR${ANSI_RED} to ${ANSI_LIGHT_RED}TRUE${ANSI_RED}."
	CONTINUE_ON_ERROR=1
fi

# Search Lattice Diamond in default installation locations
DefaultDirectories=("/usr/local/diamond" "/opt/Diamond" "/opt/diamond" "/c/Lattice/Diamond")
if [ ! -z $LSC_DIAMOND ]; then
	EnvSourceDir="$FOUNDRY/../${Lattice_Diamond_Settings[SourceDirectory]}"
else
	for DefaultDir in "${DefaultDirectories[@]}"; do
		for Major in 3; do
			for Minor in 12 11 10 9 8 7 6 5; do
				Dir=$DefaultDir/${Major}.${Minor}_x64
				if [ -d $Dir ]; then
					EnvSourceDir="$Dir/${Lattice_Diamond_Settings[SourceDirectory]}"
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
# -> $EnvSourceDir
# -> $DestDir
# <= $SourceDirectory
# <= $DestinationDirectory
SetupDirectories Lattice_Diamond "Lattice Diamond"

# create "lattice" directory and change to it
# => $DestinationDirectory
CreateDestinationDirectory
cd "$DestinationDirectory"


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


# Cleanup directory
# ==============================================================================
if [[ $CLEAN -eq 1 ]]; then
	Chapter "Cleaning up vendor directory ..."
	for Device in *; do
		VarName="DEV_${Device^^}_Enable"
		if [[ ${!VarName} -eq 1 && -d "$Device" ]]; then
			PrintVerbose "Deleting '$Device'"
			PrintDebug "rm -rf "$Device" 2> /dev/null"
			rm -rf "$Device" 2> /dev/null
		fi
	done
fi

# Excluded: pmi
#
Chapter "Searching devices ..."
# Lattice device libraries
# ==============================================================================
# EC devices
StructName="EC"
SourceDir="ec/src"
Files=(
	ORCA_CMB.vhd
	ORCA_SEQ.vhd
	ORCACOMP.vhd
	ORCA_LUT.vhd
	ORCA_MISC.vhd
	ORCA_CNT.vhd
	ORCA_IO.vhd
	ORCA_MEM.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'EC'."
	CreateLibraryStruct $StructName "ec" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
#else
#	printf "%s\n" "not found: $SourceDirectory/${Files[0]}"
fi

# ECP devices
StructName="ECP"
SourceDir="ecp/src"
Files=(
	ORCA_CMB.vhd
	ORCA_SEQ.vhd
	ORCACOMP.vhd
	ORCA_LUT.vhd
	ORCA_MISC.vhd
	ORCA_CNT.vhd
	ORCA_IO.vhd
	ORCA_MEM.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'ECP'."
	CreateLibraryStruct $StructName "ecp" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi


# ECP2 devices
StructName="ECP2"
SourceDir="ecp2/src"
Files=(
	ECP2_CMB.vhd
	ECP2_SEQ.vhd
	ECP2COMP.vhd
	ECP2_CNT.vhd
	ECP2_IO.vhd
	ECP2_LUT.vhd
	ECP2_MEM.vhd
	ECP2_MISC.vhd
	ECP2_MULT.vhd
	ECP2_SL.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'ECP2'."
	CreateLibraryStruct $StructName "ecp2" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# ECP3 devices
StructName="ECP3"
SourceDir="ecp3/src"
Files=(
	ECP3_CMB.vhd
	ECP3_SEQ.vhd
	ECP3COMP.vhd
	ECP3_CNT.vhd
	ECP3_IO.vhd
	ECP3_LUT.vhd
	ECP3_MEM.vhd
	ECP3_MISC.vhd
	ECP3_MULT.vhd
	ECP3_SL.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'ECP3'."
	CreateLibraryStruct $StructName "ecp3" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# ECP5U devices
StructName="ECP5U"
SourceDir="ecp5u/src"
Files=(
	ECP5U_CMB.vhd
	ECP5U_SEQ.vhd
	ECP5UCOMP.vhd
	ECP5U_IO.vhd
	ECP5U_LUT.vhd
	ECP5U_MEM.vhd
	ECP5U_MISC.vhd
	ECP5U_SL.vhd
	gsr_pur_assign.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'ECP5U'."
	CreateLibraryStruct $StructName "ecp5u" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# LPTM devices
StructName="LPTM"
SourceDir="lptm/src"
Files=(
	MACHXO_CMB.vhd
	MACHXO_SEQ.vhd
	MACHXOCOMP.vhd
	MACHXO_CNT.vhd
	MACHXO_IO.vhd
	MACHXO_LUT.vhd
	MACHXO_MEM.vhd
	MACHXO_MISC.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'LPTM'."
	CreateLibraryStruct $StructName "lptm" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# LPTM2 devices
StructName="LPTM2"
SourceDir="lptm2/src"
Files=(
	MACHXO2_CMB.vhd
	MACHXO2_SEQ.vhd
	MACHXO2COMP.vhd
	gsr_pur_assign.vhd
	MACHXO2_CNT.vhd
	MACHXO2_IO.vhd
	MACHXO2_LUT.vhd
	MACHXO2_MEM.vhd
	MACHXO2_MISC.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'LPTM2'."
	CreateLibraryStruct $StructName "lptm2" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# MachXO devices
StructName="MACHXO"
SourceDir="machxo/src"
Files=(
	MACHXO_CMB.vhd
	MACHXO_SEQ.vhd
	MACHXOCOMP.vhd
	MACHXO_CNT.vhd
	MACHXO_IO.vhd
	MACHXO_LUT.vhd
	MACHXO_MEM.vhd
	MACHXO_MISC.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'MachXO'."
	CreateLibraryStruct $StructName "machxo" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# MachXO2 devices
StructName="MACHXO2"
SourceDir="machxo2/src"
Files=(
	MACHXO2_CMB.vhd
	MACHXO2_SEQ.vhd
	MACHXO2COMP.vhd
	MACHXO2_CNT.vhd
	gsr_pur_assign.vhd
	MACHXO2_IO.vhd
	MACHXO2_LUT.vhd
	MACHXO2_MEM.vhd
	MACHXO2_MISC.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'MachXO2'."
	CreateLibraryStruct $StructName "machxo2" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# MachXO3L devices
StructName="MACHXO3L"
SourceDir="machxo3l/src"
Files=(
	MACHXO3L_CMB.vhd
	MACHXO3L_SEQ.vhd
	MACHXO3LCOMP.vhd
	gsr_pur_assign.vhd
	MACHXO3L_CNT.vhd
	MACHXO3L_IO.vhd
	MACHXO3L_LUT.vhd
	MACHXO3L_MEM.vhd
	MACHXO3L_MISC.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'MachXO3L'."
	CreateLibraryStruct $StructName "machxo3l" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# SC devices
StructName="SC"
SourceDir="sc/src"
Files=(
	ORCA_CMB.vhd
	ORCA_SEQ.vhd
	ORCACOMP.vhd
	ORCA_CNT.vhd
	ORCA_IO.vhd
	ORCA_MEM.vhd
	ORCA_MIS.vhd
	ORCA_SL.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'SC'."
	CreateLibraryStruct $StructName "sc" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# SCM devices
StructName="SCM"
SourceDir="scm/src"
Files=(
	ORCA_CMB.vhd
	ORCA_SEQ.vhd
	ORCACOMP.vhd
	ORCA_CNT.vhd
	ORCA_IO.vhd
	ORCA_MEM.vhd
	ORCA_MIS.vhd
	ORCA_SL.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'SCM'."
	CreateLibraryStruct $StructName "scm" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# XP devices
StructName="XP"
SourceDir="xp/src"
Files=(
	ORCA_CMB.vhd
	ORCA_SEQ.vhd
	ORCACOMP.vhd
	ORCA_LUT.vhd
	ORCA_MISC.vhd
	ORCA_CNT.vhd
	ORCA_IO.vhd
	ORCA_MEM.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'XP'."
	CreateLibraryStruct $StructName "xp" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# XP2 devices
StructName="XP2"
SourceDir="xp2/src"
Files=(
	XP2_CMB.vhd
	XP2_SEQ.vhd
	XP2COMP.vhd
	XP2_CNT.vhd
	XP2_IO.vhd
	XP2_LUT.vhd
	XP2_MEM.vhd
	XP2_MISC.vhd
	XP2_MULT.vhd
	XP2_SL.vhd
)
if [[ -f "$SourceDirectory/$SourceDir/${Files[0]}" ]]; then
	PrintDebug "Found device 'XP2'."
	CreateLibraryStruct $StructName "xp2" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
fi

# if [[ $DEBUG -eq 1 ]]; then
	# for StructName in ${Libraries[*]}; do
		# PrintLibraryStruct $StructName "    "
	# done
# fi

# Compile libraries
if [[ ${#Libraries[@]} -ne 0 ]]; then
	Compile "$SourceDirectory" "${Libraries[*]}"

	printf "%s\n" "--------------------------------------------------------------------------------"
	printf "Compiling Lattice device libraries %s\n" "$(test $ERRORCOUNT -eq 0 && echo $COLORED_SUCCESSFUL || echo $COLORED_FAILED)"
else
	PrintErrorAndExit "No Lattice device library selected." 2
fi
