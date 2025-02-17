#! /usr/bin/env bash
# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
#  Bash Script (executable):
#    Script to compile the simulation libraries from Xilinx Vivado for GHDL on
#    Linux
#
# Description:
#    - Creates a subdirectory in the current working directory
#    - Compiles all Xilinx Vivado simulation libraries and packages
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
source "$ScriptDir"/../ansi_color.sh
if [[ $? -ne 0 ]]; then
	printf "\x1b[31m[ERROR] %s\x1b[0m\n" "While loading Bash utilities." 1>&2
	exit 1
fi


# Command line argument processing
COMMAND=1
CLEAN=0
COMPILE_UNISIM=0
COMPILE_UNIMACRO=0
COMPILE_UNIFAST=0
COMPILE_SECUREIP=0
VERBOSE=0
DEBUG=0
FILTERING=1
SKIP_LARGE_FILES=0
SUPPRESS_WARNINGS=0
CONTINUE_ON_ERROR=0
VHDLStandard=93
GHDLBinDir=""
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
			CONTINUE_ON_ERROR=1
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
	printf "%s\n" "  A script to compile the Xilinx Vivado simulation libraries for GHDL on Linux."
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
	printf "%s\n" "  -a --all                     Compile all Xilinx simulation libraries."
	printf "%s\n" "     --unisim                  Compile the unisim library."
	printf "%s\n" "     --unimacro                Compile the unimacro library."
	printf "%s\n" "     --unifast                 Compile the unifast library."
	printf "%s\n" "     --with-secureip           Compile the secureip library."
	printf "\n"
	printf "%s\n" "Library compile options:"
	printf "%s\n" "     --vhdl93                  Compile the libraries with VHDL-93."
	printf "%s\n" "     --vhdl2008                Compile the libraries with VHDL-2008."
	printf "%s\n" "  -S --skip-largefiles         Don't compile large files."
	printf "%s\n" "  -H --halt-on-error           Halt on error(s)."
	printf "\n"
	printf "%s\n" "Advanced options:"
	printf "%s\n" "  --ghdl <GHDL binary>         Path to GHDL's executable, e.g. /usr/local/bin/ghdl"
	printf "%s\n" "  --output <dir name>          Name of the output directory, e.g. vivado"
	printf "%s\n" "  --source <Path to Vivado>    Path to the sources."
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
	COMPILE_UNISIM=1
	COMPILE_UNIMACRO=1
	COMPILE_UNIFAST=1
	COMPILE_SECUREIP=1
fi


# Source configuration file from GHDL's 'vendors' library directory
Chapter "Loading environment..."
source $ScriptDir/config.sh
CheckError $? "While loading configuration."

source $ScriptDir/shared.sh
CheckError $? "While loading further procedures."

# Warn that some files might not be VHDL-2008 ready. Thus enabled continue on error.
if [[ $VHDLStandard -eq 2008 ]]; then
	PrintWarning "${ANSI_RED}Not all Xilinx primitives are VHDL-2008 compatible! Setting ${ANSI_LIGHT_RED}CONTINUE_ON_ERROR${ANSI_RED} to ${ANSI_LIGHT_RED}TRUE${ANSI_RED}."
	CONTINUE_ON_ERROR=1
fi

# Search Xilinx Vivado in default installation locations
DefaultDirectories=("/opt/Xilinx/Vivado" "/opt/xilinx/Vivado" "/c/Xilinx/Vivado")
if [ ! -z $XILINX_VIVADO ]; then
	EnvSourceDir="$XILINX_VIVADO/${Xilinx_Vivado_Settings[SourceDirectory]}"
else
	for DefaultDir in "${DefaultDirectories[@]}"; do
		for Major in 2021 2020 2019 2018 2017 2016 2015 2014; do
			for Minor in 4 3 2 1; do
				Dir=$DefaultDir/${Major}.${Minor}
				if [ -d $Dir ]; then
					EnvSourceDir="$Dir/${Xilinx_Vivado_Settings[SourceDirectory]}"
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
SetupDirectories Xilinx_Vivado "Xilinx Vivado"

# create "xilinx-vivado" directory and change to it
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

# Cleanup directory
# ==============================================================================
if [[ $CLEAN -eq 1 ]]; then
	Chapter "Cleaning up vendor directory ..."

	if [[ $COMPILE_UNISIM -eq 1 && -d "unisim" ]]; then
		PrintVerbose "Deleting 'unisims'"
		PrintDebug "rm -rf "unisim" 2> /dev/null"
		rm -rf "unisim" 2> /dev/null
	fi
	if [[ $COMPILE_UNIMACRO -eq 1 && -d "unimacro" ]]; then
		PrintVerbose "Deleting 'unimacro'"
		PrintDebug "rm -rf "unimacro" 2> /dev/null"
		rm -rf "unimacro" 2> /dev/null
	fi
	if [[ $COMPILE_UNIFAST -eq 1 && -d "unifast" ]]; then
		PrintVerbose "Deleting 'unifast'"
		PrintDebug "rm -rf "unifast" 2> /dev/null"
		rm -rf "unifast" 2> /dev/null
	fi
	if [[ $COMPILE_SECUREIP -eq 1 && -d "secureip" ]]; then
		PrintVerbose "Deleting 'secureip'"
		PrintDebug "rm -rf "secureip" 2> /dev/null"
		rm -rf "secureip" 2> /dev/null
	fi
fi

# Library unisim
# ==============================================================================
Chapter "Reading compile order files..."

# Reading unisim files
StructName="UNISIM"
Library="unisim"
PrintVerbose "Reading compile order of '${Library}'"
PrintDebug "Reading compile order from '$SourceDirectory/${Library}s/primitive/vhdl_analyze_order'"

Files=(
	unisim_VPKG.vhd
	unisim_retarget_VCOMP.vhd
)
while IFS= read -r File; do
	Files+=("primitive/$File")
done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/${Library}s/primitive/vhdl_analyze_order")

# Reading unisim retarget files
PrintDebug "Reading compile order from '$SourceDirectory/${Library}s/retarget/vhdl_analyze_order'"
while IFS= read -r File; do
	Files+=("retarget/$File")
done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/${Library}s/retarget/vhdl_analyze_order")

CreateLibraryStruct $StructName $Library "${Library}s" $VHDLVersion "${Files[@]}"
test $COMPILE_UNISIM -eq 1 && Libraries+=("$StructName")

# Reading unisim secureip files
StructName="UNISIM_SECUREIP"
Library="unisim"
PrintDebug "Scanning directory '$SourceDirectory/${Library}s/secureip' for '*.vhd'"
Files=( $(cd $SourceDirectory/${Library}s/secureip; LC_COLLATE=C ls *.vhd) )

CreateLibraryStruct $StructName "secureip" "${Library}s/secureip" $VHDLVersion "${Files[@]}"
test $COMPILE_UNISIM -eq 1 && test $COMPILE_SECUREIP -eq 1 && Libraries+=("$StructName")


# Library unimacro
# ==============================================================================
# Reading unimacro files
StructName="UNIMACRO"
Library="unimacro"
PrintVerbose "Reading compile order of '${Library}'"
PrintDebug "Scanning directory '$SourceDirectory/$Library/' for '*_MACRO.vhd'"
Files=(
	unimacro_VCOMP.vhd
)
while IFS= read -r File; do
	Files+=("$File")
done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/$Library/vhdl_analyze_order")

CreateLibraryStruct $StructName $Library $Library $VHDLVersion "${Files[@]}"
test $COMPILE_UNIMACRO -eq 1 && Libraries+=("$StructName")

# Library unifast
# ==============================================================================
StructName="UNIFAST"
Library="unifast"
PrintVerbose "Reading compile order of '${Library}'"
PrintDebug "Reading compile order from '$SourceDirectory/$Library/primitive/vhdl_analyze_order'"
Files=()
while IFS= read -r File; do
	Files+=("$File")
done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/$Library/primitive/vhdl_analyze_order")

CreateLibraryStruct $StructName $Library "$Library/primitive" $VHDLVersion "${Files[@]}"
test $COMPILE_UNIFAST -eq 1 && Libraries+=("$StructName")

# Reading unifast secureip files
StructName="UNIFAST_SECUREIP"
Library="unifast"
PrintDebug "Scanning directory '$SourceDirectory/$Library/secureip' for '*.vhd'"
Files=( $(cd $SourceDirectory/$Library/secureip; LC_COLLATE=C ls *.vhd) )

CreateLibraryStruct $StructName "secureip" "$Library/secureip" $VHDLVersion "${Files[@]}"
test $COMPILE_UNIFAST -eq 1 && test $COMPILE_SECUREIP -eq 1 && Libraries+=("$StructName")


# Compile libraries
if [[ ${#Libraries[@]} -ne 0 ]]; then
	Compile "$SourceDirectory" "${Libraries[*]}"

	printf "%s\n" "--------------------------------------------------------------------------------"
	printf "Compiling Xilinx Vivado libraries %s\n" "$(test $ERRORCOUNT -eq 0 && echo $COLORED_SUCCESSFUL || echo $COLORED_FAILED)"
else
	PrintErrorAndExit "No Xilinx Vivado libraries selected." 2
fi
