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
#  Copyright (C) 2017-2021 Patrick Lehmann - Boetzingen, Germany
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
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading Bash utilities.${ANSI_NOCOLOR}"    ; exit 1; fi


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
				echo 1>&2 -e "\n${COLORED_ERROR} Unknown command line option '$1'.${ANSI_NOCOLOR}"
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
	echo "  -h --help                    Print this help page"
	echo "  -c --clean                   Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all                     Compile all Lattice simulation libraries."
	for Device in $DeviceList; do
	  printf "     --%-23s Device primitives for '%s'.\n" "${Device,,}" "$Device"
	done
	echo ""
	echo "Library compile options:"
	echo "     --vhdl93                  Compile the libraries with VHDL-93."
	echo "     --vhdl2008                Compile the libraries with VHDL-2008."
	echo "  -H --halt-on-error           Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "  --ghdl <GHDL binary>         Path to GHDL's executable, e.g. /usr/local/bin/ghdl"
	echo "  --output <dir name>          Name of the output directory, e.g. lattice"
	echo "  --source <Path to Diamond>   Path to the sources."
	echo ""
	echo "Verbosity:"
	echo "  -v --verbose                 Print verbose messages."
	echo "  -d --debug                   Print debug messages."
	echo "  -n --no-filter               Disable output filtering scripts."
	echo "  -N --no-warnings             Suppress all warnings. Show only error messages."
	echo ""
	exit $COMMAND
fi

if [[ $COMMAND -eq 2 ]]; then
	for Device in $DeviceList; do
		declare "DEV_${Device}_Enable"=1
	done
fi


# Source configuration file from GHDL's 'vendors' library directory
echo -e "${ANSI_MAGENTA}Loading environment...${ANSI_NOCOLOR}"
source $ScriptDir/config.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading configuration.${ANSI_NOCOLOR}"     ; exit 1; fi
source $ScriptDir/shared.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading further procedures.${ANSI_NOCOLOR}"; exit 1; fi

# Warn that some files might not be VHDL-2008 ready. Thus enabled continue on error.
if [[ $VHDLStandard -eq 2008 ]]; then
	echo -e "${ANSI_RED}Not all Lattice packages are VHDL-2008 compatible! Setting CONTINUE_ON_ERROR to TRUE.${ANSI_NOCOLOR}"
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
	echo 1>&2 -e "${COLORED_ERROR} '--clean' is not implemented!"
	exit 1
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_NOCOLOR}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi

# Excluded: pmi
#
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'EC'.${ANSI_NOCOLOR}"
	CreateLibraryStruct $StructName "ec" $SourceDir $VHDLVersion "${Files[@]}"

	VarName="DEV_${StructName}_Enable"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
#else
#	echo "not found: $SourceDirectory/${Files[0]}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'ECP'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'ECP2'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'ECP3'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'ECP5U'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'LPTM'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'LPTM2'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'MachXO'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'MachXO2'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'MachXO3L'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'SC'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'SCM'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'XP'.${ANSI_NOCOLOR}"
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
	test $DEBUG -eq 1 && echo -e "    ${ANSI_DARK_GRAY}Found device 'XP2'.${ANSI_NOCOLOR}"
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

	echo "--------------------------------------------------------------------------------"
	echo -e "Compiling Lattice device libraries $(test $ERRORCOUNT -eq 0 && echo $COLORED_SUCCESSFUL || echo $COLORED_FAILED)"
else
	echo -e "${ANSI_RED}No Lattice device library selected.${ANSI_NOCOLOR}"
fi
