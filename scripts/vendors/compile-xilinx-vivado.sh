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
#  Copyright (C) 2017-2021 Patrick Lehmann - Boetzingen, Germany
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
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading Bash utilities.${ANSI_NOCOLOR}"    ; exit 1; fi


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
			echo 1>&2 -e "\n${COLORED_ERROR} Unknown command line option '$1'.${ANSI_NOCOLOR}"
			COMMAND=0
			break
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
	echo "  A script to compile the Xilinx Vivado simulation libraries for GHDL on Linux."
	echo "  One library folder 'lib/v??' per VHDL library will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	echo ""
	echo "Usage:"
	echo "  compile-xilinx-vivado.sh <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help                    Print this help page"
	echo "  -c --clean                   Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all                     Compile all Xilinx simulation libraries."
	echo "     --unisim                  Compile the unisim library."
	echo "     --unimacro                Compile the unimacro library."
	echo "     --unifast                 Compile the unifast library."
	echo "     --with-secureip           Compile the secureip library."
	echo ""
	echo "Library compile options:"
	echo "     --vhdl93                  Compile the libraries with VHDL-93."
	echo "     --vhdl2008                Compile the libraries with VHDL-2008."
	echo "  -S --skip-largefiles         Don't compile large files."
	echo "  -H --halt-on-error           Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "  --ghdl <GHDL binary>         Path to GHDL's executable, e.g. /usr/local/bin/ghdl"
	echo "  --output <dir name>          Name of the output directory, e.g. vivado"
	echo "  --source <Path to Vivado>    Path to the sources."
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
	COMPILE_UNISIM=1
	COMPILE_UNIMACRO=1
	COMPILE_UNIFAST=1
	COMPILE_SECUREIP=1
fi


# Source configuration file from GHDL's 'vendors' library directory
echo -e "${ANSI_MAGENTA}Loading environment...${ANSI_NOCOLOR}"
source $ScriptDir/config.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading configuration.${ANSI_NOCOLOR}"     ; exit 1; fi
source $ScriptDir/shared.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading further procedures.${ANSI_NOCOLOR}"; exit 1; fi

# Warn that some files might not be VHDL-2008 ready. Thus enabled continue on error.
if [[ $VHDLStandard -eq 2008 ]]; then
	echo -e "${ANSI_RED}Not all Xilinx primitives are VHDL-2008 compatible! Setting CONTINUE_ON_ERROR to TRUE.${ANSI_NOCOLOR}"
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
	echo 1>&2 -e "${COLORED_ERROR} '--clean' is not implemented!"
	exit 1
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_NOCOLOR}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi

# Library unisim
# ==============================================================================
test $VERBOSE -eq 1 && echo -e "  ${ANSI_GRAY}Reading compile order files...${ANSI_NOCOLOR}"

# Reading unisim files
StructName="UNISIM"
Library="unisim"
test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Reading compile order from '$SourceDirectory/${Library}s/primitive/vhdl_analyze_order'${ANSI_NOCOLOR}"
Files=(
	unisim_VPKG.vhd
	unisim_retarget_VCOMP.vhd
)
while IFS= read -r File; do
	Files+=("primitive/$File")
done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/${Library}s/primitive/vhdl_analyze_order")

# Reading unisim retarget files
test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Reading compile order from '$SourceDirectory/${Library}s/retarget/vhdl_analyze_order'${ANSI_NOCOLOR}"
while IFS= read -r File; do
	Files+=("retarget/$File")
done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/${Library}s/retarget/vhdl_analyze_order")

CreateLibraryStruct $StructName $Library "${Library}s" $VHDLVersion "${Files[@]}"
test $COMPILE_UNISIM -eq 1 && Libraries+=("$StructName")

# Reading unisim secureip files
StructName="UNISIM_SECUREIP"
Library="unisim"
test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Scanning directory '$SourceDirectory/${Library}s/secureip' for '*.vhd'${ANSI_NOCOLOR}"
Files=( $(cd $SourceDirectory/${Library}s/secureip; LC_COLLATE=C ls *.vhd) )

CreateLibraryStruct $StructName "secureip" "${Library}s/secureip" $VHDLVersion "${Files[@]}"
test $COMPILE_UNISIM -eq 1 && test $COMPILE_SECUREIP -eq 1 && Libraries+=("$StructName")


# Library unimacro
# ==============================================================================
# Reading unimacro files
StructName="UNIMACRO"
Library="unimacro"
test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Scanning directory '$SourceDirectory/$Library/' for '*_MACRO.vhd'${ANSI_NOCOLOR}"
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
test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Reading compile order from '$SourceDirectory/$Library/primitive/vhdl_analyze_order'${ANSI_NOCOLOR}"
Files=()
while IFS= read -r File; do
	Files+=("$File")
done < <(grep --no-filename -R '^[a-zA-Z]' "$SourceDirectory/$Library/primitive/vhdl_analyze_order")

CreateLibraryStruct $StructName $Library "$Library/primitive" $VHDLVersion "${Files[@]}"
test $COMPILE_UNIFAST -eq 1 && Libraries+=("$StructName")

# Reading unifast secureip files
StructName="UNIFAST_SECUREIP"
Library="unifast"
test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Scanning directory '$SourceDirectory/$Library/secureip' for '*.vhd'${ANSI_NOCOLOR}"
Files=( $(cd $SourceDirectory/$Library/secureip; LC_COLLATE=C ls *.vhd) )

CreateLibraryStruct $StructName "secureip" "$Library/secureip" $VHDLVersion "${Files[@]}"
test $COMPILE_UNIFAST -eq 1 && test $COMPILE_SECUREIP -eq 1 && Libraries+=("$StructName")


# Compile libraries
if [[ ${#Libraries[@]} -ne 0 ]]; then
	Compile "$SourceDirectory" "${Libraries[*]}"

	echo "--------------------------------------------------------------------------------"
	echo -e "Compiling Xilinx Vivado libraries $(test $ERRORCOUNT -eq 0 && echo $COLORED_SUCCESSFUL || echo $COLORED_FAILED)"
else
	echo -e "${ANSI_RED}No Xilinx Vivado libraries selected.${ANSI_NOCOLOR}"
fi
