#! /usr/bin/env bash
# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
#  Bash Script (executable):
#    Script to compile the UVVM library for GHDL on Linux.
#
#  Description:
#    - Creates a subdirectory in the current working directory
#    - Compiles all UVVM packages and verification IPs
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
source $ScriptDir/../ansi_color.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading Bash utilities.${ANSI_NOCOLOR}"    ; exit 1; fi


# Command line argument processing
COMMAND=1
CLEAN=0
COMPILE_UVVM=0
COMPILE_UVVM_UTILITIES=0
COMPILE_UVVM_VVC_FRAMEWORK=0
COMPILE_UVVM_VIP=0
COMPILE_UVVM_VIP_AVALON_MM=0
COMPILE_UVVM_VIP_AVALON_ST=0
COMPILE_UVVM_VIP_AXI=0
COMPILE_UVVM_VIP_AXI_LITE=0
COMPILE_UVVM_VIP_AXI_STREAM=0
COMPILE_UVVM_VIP_CLOCK_GENERATOR=0
COMPILE_UVVM_VIP_ERROR_INJECTION=0
COMPILE_UVVM_VIP_ETHERNET=0
COMPILE_UVVM_VIP_GMII=0
COMPILE_UVVM_VIP_GPIO=0
COMPILE_UVVM_VIP_HVVC_TO_VVC_BRIDGE=0
COMPILE_UVVM_VIP_I2C=0
COMPILE_UVVM_VIP_RGMII=0
COMPILE_UVVM_VIP_SBI=0
COMPILE_UVVM_VIP_SCOREBOARD=0
COMPILE_UVVM_VIP_SPEC_COV=0
COMPILE_UVVM_VIP_SPI=0
COMPILE_UVVM_VIP_UART=0
COMPILE_UVVM_VIP_WISHBONE=0
VERBOSE=0
DEBUG=0
FILTERING=1
SUPPRESS_WARNINGS=0
HALT_ON_ERROR=0
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
		--uvvm)
			COMMAND=3
			COMPILE_UVVM=1
			;;
		--uvvm-vip)
			COMMAND=3
			COMPILE_UVVM_VIP=1
			;;
		--uvvm-utilities)
			COMMAND=3
			COMPILE_UVVM_UTILITIES=1
			;;
		--uvvm-vvc-framework)
			COMMAND=3
			COMPILE_UVVM_VVC_FRAMEWORK=1
			;;
		--uvvm-vip-avalon_mm)
			COMMAND=3
			COMPILE_UVVM_VIP_AVALON_MM=1
			;;
		--uvvm-vip-avalon_st)
			COMMAND=3
			COMPILE_UVVM_VIP_AVALON_ST=1
			;;
		--uvvm-vip-axi)
			COMMAND=3
			COMPILE_UVVM_VIP_AXI=1
			;;
		--uvvm-vip-axi_lite)
			COMMAND=3
			COMPILE_UVVM_VIP_AXI_LITE=1
			;;
		--uvvm-vip-axi_stream)
			COMMAND=3
			COMPILE_UVVM_VIP_AXI_STREAM=1
			;;
		--uvvm-vip-clock)
			COMMAND=3
			COMPILE_UVVM_VIP_CLOCK_GENERATOR=1
			;;
		--uvvm-vip-error)
			COMMAND=3
			COMPILE_UVVM_VIP_ERROR_INJECTION=1
			;;
		--uvvm-vip-ethernet)
			COMMAND=3
			COMPILE_UVVM_VIP_ETHERNET=1
			;;
		--uvvm-vip-gmii)
			COMMAND=3
			COMPILE_UVVM_VIP_GMII=1
			;;
		--uvvm-vip-gpio)
			COMMAND=3
			COMPILE_UVVM_VIP_GPIO=1
			;;
		--uvvm-vip-hvvc2vvc)
			COMMAND=3
			COMPILE_UVVM_VIP_HVVC_TO_VVC_BRIDGE=1
			;;
		--uvvm-vip-i2c)
			COMMAND=3
			COMPILE_UVVM_VIP_I2C=1
			;;
		--uvvm-vip-rgmii)
			COMMAND=3
			COMPILE_UVVM_VIP_RGMII=1
			;;
		--uvvm-vip-sbi)
			COMMAND=3
			COMPILE_UVVM_VIP_SBI=1
			;;
		--uvvm-vip-spec)
			COMMAND=3
			COMPILE_UVVM_VIP_SPEC_COV=1
			;;
		--uvvm-vip-spi)
			COMMAND=3
			COMPILE_UVVM_VIP_SPI=1
			;;
		--uvvm-vip-scoreboard)
			COMMAND=3
			COMPILE_UVVM_VIP_SCOREBOARD=1
			;;
		--uvvm-vip-uart)
			COMMAND=3
			COMPILE_UVVM_VIP_UART=1
			;;
		--uvvm-vip-wishbone)
			COMMAND=3
			COMPILE_UVVM_VIP_WISHBONE=1
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
			echo 1>&2 -e "\n${COLORED_ERROR} Unknown command line option '$1'.${ANSI_NOCOLOR}"
			COMMAND=0
			break
			;;
	esac
	shift # parsed argument or value
done

ERRORCOUNT=0

if [[ $COMMAND -le 1 ]]; then
	test $COMMAND -eq 1 && echo 1>&2 -e "\n${COLORED_ERROR} No command selected.${ANSI_NOCOLOR}"
	echo ""
	echo "Synopsis:"
	echo "  A script to compile the simulation library 'uvvm' for GHDL on Linux."
	echo "  A library folder 'uvvm/v08' will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default parameters."
	echo ""
	echo "Usage:"
	echo "  compile-uvvm.sh [<verbosity>] <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help                    Print this help page"
	echo "  -c --clean                   Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all                     Compile all libraries."
	echo "     --uvvm                    Compile UVVM library packages."
	echo "     --uvvm-vip                Compile UVVM Verification IPs (VIPs)."
	echo ""
	echo "Common Packages:"
	echo "     --uvvm-utilities          UVVM utilities."
	echo "     --uvvm-vvc-framework      VHDL Verification Component (VVC) framework."
	echo ""
	echo "Verification IPs:"
	echo "     --uvvm-vip-avalon_mm      Altera/Intel Avalon Memory Mapped"
	echo "     --uvvm-vip-avalon_st      Altera/Intel Avalon Stream"
	echo "     --uvvm-vip-axi            ARM AMBA AXI4"
	echo "     --uvvm-vip-axi_lite       ARM AMBA AXI4-Lite"
	echo "     --uvvm-vip-axi_stream     ARM AMBA AXI4-Stream"
	echo "     --uvvm-vip-clock          Clock generator"
	echo "     --uvvm-vip-error          Error injection"
	echo "     --uvvm-vip-ethernet       Ethernet"
	echo "     --uvvm-vip-gmii           GMII"
	echo "     --uvvm-vip-gpio           General Purpose Input/Output (GPIO)"
	echo "     --uvvm-vip-hvvc2vvc       HVVC to VVC bridge"
	echo "     --uvvm-vip-i2c            Inter-Integrated Circuit (I²C)"
	echo "     --uvvm-vip-rgmii          RGMII"
	echo "     --uvvm-vip-sbi            Simple Bus Interface"
	echo "     --uvvm-vip-scoreboard     Scoreboard"
	echo "     --uvvm-vip-spec           Specification Coverage"
	echo "     --uvvm-vip-spi            Serial Peripheral Interface"
	echo "     --uvvm-vip-uart           Universal Asynchronous Receiver Transmitter (UART)"
	echo "     --uvvm-vip-wishbone       Wishbone"
	echo ""
	echo "Library compile options:"
	echo "  -H --halt-on-error           Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "  --ghdl <GHDL binary>         Path to GHDL's executable, e.g. /usr/local/bin/ghdl"
	echo "  --output <dir name>          Name of the output directory, e.g. uvvm_util"
	echo "  --source <Path to UVVM>      Path to the sources."
	echo ""
	echo "Verbosity:"
	echo "  -v --verbose                  Print verbose messages."
	echo "  -d --debug                    Print debug messages."
	echo "  -n --no-filter                Disable output filtering scripts."
	echo "  -N --no-warnings              Suppress all warnings. Show only error messages."
	echo ""
	exit $COMMAND
fi

if [[ $COMMAND -eq 2 ]]; then
	COMPILE_UVVM=1
	COMPILE_UVVM_VIP=1
fi
if [[ $COMPILE_UVVM -eq 1 ]]; then
	COMPILE_UVVM_UTILITIES=1
	COMPILE_UVVM_VVC_FRAMEWORK=1
fi
if [[ $COMPILE_UVVM_VIP -eq 1 ]]; then
	COMPILE_UVVM_VIP_AVALON_MM=1
	COMPILE_UVVM_VIP_AVALON_ST=1
	COMPILE_UVVM_VIP_AXI=1
	COMPILE_UVVM_VIP_AXI_LITE=1
	COMPILE_UVVM_VIP_AXI_STREAM=1
	COMPILE_UVVM_VIP_CLOCK_GENERATOR=1
	COMPILE_UVVM_VIP_ERROR_INJECTION=1
	COMPILE_UVVM_VIP_ETHERNET=1
	COMPILE_UVVM_VIP_GMII=1
	COMPILE_UVVM_VIP_GPIO=1
	COMPILE_UVVM_VIP_HVVC_TO_VVC_BRIDGE=1
	COMPILE_UVVM_VIP_I2C=1
	COMPILE_UVVM_VIP_RGMII=1
	COMPILE_UVVM_VIP_SBI=1
	COMPILE_UVVM_VIP_SCOREBOARD=1
	COMPILE_UVVM_VIP_SPEC_COV=1
	COMPILE_UVVM_VIP_SPI=1
	COMPILE_UVVM_VIP_UART=1
	COMPILE_UVVM_VIP_WISHBONE=1
fi


# Source configuration file from GHDL's 'vendors' library directory
echo -e "${ANSI_MAGENTA}Loading environment...${ANSI_NOCOLOR}"
source $ScriptDir/config.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading configuration.${ANSI_NOCOLOR}"     ; exit 1; fi
source $ScriptDir/shared.sh
if [[ $? -ne 0 ]]; then echo 1>&2 -e "${COLORED_ERROR} While loading further procedures.${ANSI_NOCOLOR}"; exit 1; fi

# <= $VHDLVersion
# <= $VHDLStandard
# <= $VHDLFlavor
GHDLSetup 2008

# -> $SourceDirectories
# -> $DestinationDirectories
# -> $SrcDir
# -> $DestDir
# <= $SourceDirectory
# <= $DestinationDirectory
SetupDirectories UVVM "UVVM"

# Create "uvvm" directory and change to it
# => $DestinationDirectory
CreateDestinationDirectory
cd $DestinationDirectory


# Extend global GHDL Options TODO: move to GHDLSetup
Analyze_Parameters+=(
	-fexplicit
	-Wbinding
	-Wno-shared
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
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_NOCOLOR}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi


# Read order of components
# ==============================================================================
test $VERBOSE -eq 1 && echo -e "  ${ANSI_GRAY}Reading compile order files...${ANSI_NOCOLOR}"

Components=()
while IFS=$'\r\n' read -r Component; do
	if [[ ${Component:0:2} != "# " ]]; then
		Components+=("$Component")
	fi
done < <(cat "$SourceDirectory/script/component_list.txt")


# Read libraries and Verification IPs
# ==============================================================================
VIPNames=()

for ComponentName in "${Components[@]}"; do
	x="${ComponentName%%_*}"
	pos=${#x}+1
	l=${ComponentName:$pos}
	VIPName=${l^^}
	LibraryPath=$ComponentName

	VIPName=${VIPName//UTIL/UTILITIES}
	VIPName=${VIPName//AXILITE/AXI_LITE}
	VIPName=${VIPName//AXISTREAM/AXI_STREAM}

	test $VERBOSE -eq 1 && echo -e "  ${ANSI_GRAY}Found VIP '$VIPName' in '$LibraryPath'.${ANSI_NOCOLOR}"
	test $DEBUG -eq 1   && echo -e "    ${ANSI_DARK_GRAY}Reading compile order from '$SourceDirectory/$LibraryPath/script/compile_order.txt'${ANSI_NOCOLOR}"

	# Reading component's files
	StructName=$VIPName
	Files=()

	CompileOrderFile="$SourceDirectory/$LibraryPath/script/compile_order.txt"
	if [ ! -f "$CompileOrderFile" ]; then
	  echo -e "${COLORED_ERROR} Compile order file '$CompileOrderFile' does not exist..${ANSI_NOCOLOR}"
	  continue
	fi

	while IFS=$'\r\n' read -r File; do
		if [[ ${File:0:2} == "# " ]]; then
			if [[ ${File:2:7} == "library" ]]; then
				LibraryName=${File:10}
			fi
		else
			Files+=("${File:3}")
		fi
	done < <(cat "$CompileOrderFile")

	CreateLibraryStruct $StructName $LibraryName $LibraryPath $VHDLVersion "${Files[@]}"

	VarName="COMPILE_UVVM_${VIPName}"
	test ${!VarName} -eq 1 && Libraries+=("$StructName")
done

# Compile components
if [[ ${#Libraries[@]} -ne 0 ]]; then
	Compile "$SourceDirectory" "${Libraries[*]}"

	echo "--------------------------------------------------------------------------------"
	echo -e "Compiling UVVM packages and VIPs $(test $ERRORCOUNT -eq 0 && echo $COLORED_SUCCESSFUL || echo $COLORED_FAILED)"
else
	echo -e "${COLORED_ERROR} Neither UVVM packages nor VIPs selected.${ANSI_NOCOLOR}"
	exit 2
fi
