#! /usr/bin/env bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	Bash Script:				Script to compile the UVVM library for GHDL on Linux
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all UVVM packages 
#
# ==============================================================================
#	Copyright (C) 2015-2017 Patrick Lehmann - Dresden, Germany
#	Copyright (C) 2017 Patrick Lehmann - Freiburg, Germany
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
# work around for Darwin (Mac OS)
READLINK=readlink; if [[ $(uname) == "Darwin" ]]; then READLINK=greadlink; fi

# save working directory
WorkingDir=$(pwd)
ScriptDir="$(dirname $0)"
ScriptDir="$($READLINK -f $ScriptDir)"

# source configuration file from GHDL's 'vendors' library directory
. $ScriptDir/../ansi_color.sh
. $ScriptDir/config.sh
. $ScriptDir/shared.sh

# command line argument processing
NO_COMMAND=1
SUPPRESS_WARNINGS=0
HALT_ON_ERROR=0
GHDLBinDir=""
DestDir=""
SrcDir=""
while [[ $# > 0 ]]; do
	key="$1"
	case $key in
		-c|--clean)
		CLEAN=TRUE
		NO_COMMAND=0
		;;
		-a|--all)
		COMPILE_ALL=TRUE
		NO_COMMAND=0
		;;
		--uvvm)
		COMPILE_UVVM=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip)
		COMPILE_UVVM_VIP=TRUE
		NO_COMMAND=0
		;;
		--uvvm-utilities)
		COMPILE_UVVM_UTILITIES=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vvc-framework)
		COMPILE_UVVM_VVC_FRAMEWORK=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-avalon_mm)
		COMPILE_UVVM_VIP_AVALON_MM=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-axi_lite)
		COMPILE_UVVM_VIP_AXILITE=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-axi_stream)
		COMPILE_UVVM_VIP_AXISTREAM=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-gpio)
		COMPILE_UVVM_VIP_GPIO=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-i2c)
		COMPILE_UVVM_VIP_I2C=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-sbi)
		COMPILE_UVVM_VIP_SBI=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-spi)
		COMPILE_UVVM_VIP_SPI=TRUE
		NO_COMMAND=0
		;;
		--uvvm-vip-uart)
		COMPILE_UVVM_VIP_UART=TRUE
		NO_COMMAND=0
		;;
		-h|--help)
		HELP=TRUE
		NO_COMMAND=0
		;;
		-n|--no-warnings)
		SUPPRESS_WARNINGS=1
		;;
		-H|--halt-on-error)
		HALT_ON_ERROR=1
		;;
		--ghdl)
		GHDLBinDir="$2"
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
		echo 1>&2 -e "${COLORED_ERROR} Unknown command line option '$key'.${ANSI_NOCOLOR}"
		exit -1
		;;
	esac
	shift # past argument or value
done

# makes no sense to enable it for UVVM
SKIP_EXISTING_FILES=0

if [ $NO_COMMAND -eq 1 ]; then
	HELP=TRUE
fi

if [ "$HELP" == "TRUE" ]; then
	test $NO_COMMAND -eq 1 && echo 1>&2 -e "/n${COLORED_ERROR} No command selected."
	echo ""
	echo "Synopsis:"
	echo "  A script to compile the simulation library 'uvvm_util' for GHDL on Linux."
	echo "  A library folder 'uvvm_util/v08' will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	echo ""
	echo "Usage:"
	echo "  compile-uvvm.sh <common command>|<library> [<options>] [<adv. options>]"
	echo ""
	echo "Common commands:"
	echo "  -h --help             Print this help page"
	echo "  -c --clean            Remove all generated files"
	echo ""
	echo "Libraries:"
	echo "  -a --all              Compile all libraries."
	echo "     --uvvm             Compile UVVM library packages."
	echo "     --uvvm-vip         Compile UVVM Verification IPs (VIPs)."
	echo ""
	echo "Common Packages:"
	echo "     --uvvm-utilities      "
	echo "     --uvvm-vvc-framework  "
	echo ""
	echo "Verification IPs:"
	echo "     --uvvm-vip-avalon_mm  "
	echo "     --uvvm-vip-axi_lite   "
	echo "     --uvvm-vip-axi_stream "
	echo "     --uvvm-vip-gpio       "
	echo "     --uvvm-vip-i2c        "
	echo "     --uvvm-vip-sbi        "
	echo "     --uvvm-vip-spi        "
	echo "     --uvvm-vip-uart       "
	echo ""
	echo "Library compile options:"
	echo "  -H --halt-on-error    Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "  --ghdl <GHDL bin dir> Path to GHDL's binary directory, e.g. /usr/local/bin"
	echo "  --out <dir name>      Name of the output directory, e.g. uvvm_util"
	echo "  --src <Path to UVVM>  Path to the sources."
	echo ""
	echo "Verbosity:"
	echo "  -n --no-warnings      Suppress all warnings. Show only error messages."
	echo ""
	exit 0
fi

if [ "$COMPILE_ALL" == "TRUE" ]; then
	COMPILE_UVVM=TRUE
	COMPILE_UVVM_VIP=TRUE
fi
if [ "$COMPILE_UVVM" == "TRUE" ]; then
	COMPILE_UVVM_UTILITIES=TRUE
	COMPILE_UVVM_VVC_FRAMEWORK=TRUE
fi
if [ "$COMPILE_UVVM_VIP" == "TRUE" ]; then
	COMPILE_UVVM_VIP_AVALON_MM=TRUE
	COMPILE_UVVM_VIP_AXILITE=TRUE
	COMPILE_UVVM_VIP_AXISTREAM=TRUE
	COMPILE_UVVM_VIP_GPIO=TRUE
	COMPILE_UVVM_VIP_I2C=TRUE
	COMPILE_UVVM_VIP_SBI=TRUE
	COMPILE_UVVM_VIP_SPI=TRUE
	COMPILE_UVVM_VIP_UART=TRUE
fi

# -> $SourceDirectories
# -> $DestinationDirectories
# -> $SrcDir
# -> $DestDir
# -> $GHDLBinDir
# <= $SourceDirectory
# <= $DestinationDirectory
# <= $GHDLBinary
SetupDirectories UVVM "UVVM"

# create "uvvm_util" directory and change to it
# => $DestinationDirectory
CreateDestinationDirectory
cd $DestinationDirectory


# => $SUPPRESS_WARNINGS
# <= $GRC_COMMAND
SetupGRCat


# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)

# create a set of GHDL parameters
GHDL_PARAMS=(${GHDL_OPTIONS[@]})
GHDL_PARAMS+=(--std=08 -P$DestinationDirectory)

# Cleanup directory
# ==============================================================================
if [ "$CLEAN" == "TRUE" ]; then
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_NOCOLOR}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi

# UVVM libraries
# ==============================================================================
# compile uvvm_util packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_UTILITIES" == "TRUE" ]; then
	Library="uvvm_util"
	VHDLVersion="v08"
	Files=(
		uvvm_util/src/types_pkg.vhd
		uvvm_util/src/adaptations_pkg.vhd
		uvvm_util/src/string_methods_pkg.vhd
		uvvm_util/src/protected_types_pkg.vhd
		uvvm_util/src/global_signals_and_shared_variables_pkg.vhd
		uvvm_util/src/hierarchy_linked_list_pkg.vhd
		uvvm_util/src/alert_hierarchy_pkg.vhd
		uvvm_util/src/license_pkg.vhd
		uvvm_util/src/methods_pkg.vhd
		uvvm_util/src/bfm_common_pkg.vhd
		uvvm_util/src/uvvm_util_context.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile uvvm_vvc_framework packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_VVC_FRAMEWORK" == "TRUE" ]; then
	Library="uvvm_vvc_framework"
	VHDLVersion="v08"
	Files=(
		uvvm_vvc_framework/src/ti_vvc_framework_support_pkg.vhd
		uvvm_vvc_framework/src/ti_generic_queue_pkg.vhd
		uvvm_vvc_framework/src/ti_data_queue_pkg.vhd
		uvvm_vvc_framework/src/ti_data_fifo_pkg.vhd
		uvvm_vvc_framework/src/ti_data_stack_pkg.vhd
		uvvm_vvc_framework/src/ti_uvvm_engine.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# Verification IPs
# ==============================================================================
# compile bitvis_vip_avalon_mm packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_VIP_AVALON_MM" == "TRUE" ]; then
	Library="bitvis_vip_avalon_mm"
	VHDLVersion="v08"
	Files=(
		bitvis_vip_avalon_mm/src/avalon_mm_bfm_pkg.vhd
		bitvis_vip_avalon_mm/src/vvc_cmd_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
		bitvis_vip_avalon_mm/src/vvc_methods_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
		bitvis_vip_avalon_mm/src/avalon_mm_vvc.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile bitvis_vip_axilite packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_VIP_AXILITE" == "TRUE" ]; then
	Library="bitvis_vip_axilite"
	VHDLVersion="v08"
	Files=(
		bitvis_vip_axilite/src/axilite_bfm_pkg.vhd
		bitvis_vip_axilite/src/vvc_cmd_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
		bitvis_vip_axilite/src/vvc_methods_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
		bitvis_vip_axilite/src/axilite_vvc.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile bitvis_vip_axistream packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_VIP_AXISTREAM" == "TRUE" ]; then
	Library="bitvis_vip_axistream"
	VHDLVersion="v08"
	Files=(
		bitvis_vip_axistream/src/axistream_bfm_pkg.vhd
		bitvis_vip_axistream/src/vvc_cmd_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
		bitvis_vip_axistream/src/vvc_methods_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
		bitvis_vip_axistream/src/axistream_vvc.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile bitvis_vip_gpio packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_VIP_GPIO" == "TRUE" ]; then
	Library="bitvis_vip_gpio"
	VHDLVersion="v08"
	Files=(
		bitvis_vip_gpio/src/gpio_bfm_pkg.vhd
		bitvis_vip_gpio/src/vvc_cmd_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
		bitvis_vip_gpio/src/vvc_methods_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
		bitvis_vip_gpio/src/gpio_vvc.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile bitvis_vip_i2c packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_VIP_I2C" == "TRUE" ]; then
	Library="bitvis_vip_i2c"
	VHDLVersion="v08"
	Files=(
		bitvis_vip_i2c/src/i2c_bfm_pkg.vhd
		bitvis_vip_i2c/src/vvc_cmd_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
		bitvis_vip_i2c/src/vvc_methods_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
		bitvis_vip_i2c/src/i2c_vvc.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile bitvis_vip_sbi packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_VIP_SBI" == "TRUE" ]; then
	Library="bitvis_vip_sbi"
	VHDLVersion="v08"
	Files=(
		bitvis_vip_sbi/src/sbi_bfm_pkg.vhd
		bitvis_vip_sbi/src/vvc_cmd_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
		bitvis_vip_sbi/src/vvc_methods_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
		bitvis_vip_sbi/src/sbi_vvc.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile bitvis_vip_spi packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_VIP_SPI" == "TRUE" ]; then
	Library="bitvis_vip_spi"
	VHDLVersion="v08"
	Files=(
		bitvis_vip_spi/src/spi_bfm_pkg.vhd
		bitvis_vip_spi/src/vvc_cmd_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
		bitvis_vip_spi/src/vvc_methods_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
		bitvis_vip_spi/src/spi_vvc.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile bitvis_vip_uart packages
ERRORCOUNT=0
if [ "$COMPILE_UVVM_VIP_UART" == "TRUE" ]; then
	Library="bitvis_vip_uart"
	VHDLVersion="v08"
	Files=(
		bitvis_vip_uart/src/uart_bfm_pkg.vhd
		bitvis_vip_uart/src/vvc_cmd_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_target_support_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_framework_common_methods_pkg.vhd
		bitvis_vip_uart/src/vvc_methods_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_queue_pkg.vhd
		uvvm_vvc_framework/src_target_dependent/td_vvc_entity_support_pkg.vhd
		bitvis_vip_uart/src/uart_rx_vvc.vhd
		bitvis_vip_uart/src/uart_tx_vvc.vhd
		bitvis_vip_uart/src/uart_vvc.vhd
	)

	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi
	
echo "--------------------------------------------------------------------------------"
echo -n "Compiling UVVM packages "
if [ $ERRORCOUNT -gt 0 ]; then
	echo -e $COLORED_FAILED
else
	echo -e $COLORED_SUCCESSFUL
fi
