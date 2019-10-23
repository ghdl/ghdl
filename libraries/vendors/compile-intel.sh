#! /usr/bin/env bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	Bash Script:				Script to compile the simulation libraries from Altera
#											Quartus for GHDL on Linux
# 
# Description:
# ------------------------------------
#	This is a Bash script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Altera Quartus-II simulation libraries and packages
#
# ==============================================================================
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
SKIP_EXISTING_FILES=0
SKIP_LARGE_FILES=0
SUPPRESS_WARNINGS=0
HALT_ON_ERROR=0
VHDLStandard=93
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
		--altera)
		COMPILE_ALTERA=TRUE
		NO_COMMAND=0
		;;
		--max)
		COMPILE_MAX=TRUE
		NO_COMMAND=0
		;;
		--cyclone)
		COMPILE_CYCLONE=TRUE
		NO_COMMAND=0
		;;
		--arria)
		COMPILE_ARRIA=TRUE
		NO_COMMAND=0
		;;
		--stratix)
		COMPILE_STRATIX=TRUE
		NO_COMMAND=0
		;;
		--nanometer)
		COMPILE_NM=TRUE
		NO_COMMAND=0
		;;
		-h|--help)
		HELP=TRUE
		NO_COMMAND=0
		;;
		-s|--skip-existing)
		SKIP_EXISTING_FILES=1
		;;
		-S|--skip-largefiles)
		SKIP_LARGE_FILES=1
		;;
		-n|--no-warnings)
		SUPPRESS_WARNINGS=1
		;;
		-H|--halt-on-error)
		HALT_ON_ERROR=1
		;;
		--vhdl93)
		VHDLStandard=93
		;;
		--vhdl2008)
		VHDLStandard=2008
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

if [ $NO_COMMAND -eq 1 ]; then
	HELP=TRUE
fi

if [ "$HELP" == "TRUE" ]; then
	test $NO_COMMAND -eq 1 && echo 1>&2 -e "\n${COLORED_ERROR} No command selected."
	echo ""
	echo "Synopsis:"
	echo "  A script to compile the Altera Quartus simulation libraries for GHDL on Linux."
	echo "  One library folder 'lib/v??' per VHDL library will be created relative to the current"
	echo "  working directory."
	echo ""
	echo "  Use the adv. options or edit 'config.sh' to supply paths and default params."
	echo ""
	echo "Usage:"
	echo "  compile-altera.sh <common command>|<library> [<options>] [<adv. options>]"
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
	echo "  -S --skip-largefiles  Don't compile large files. Exclude *HSSI* and *HIP* files."
	echo "  -H --halt-on-error    Halt on error(s)."
	echo ""
	echo "Advanced options:"
	echo "  --ghdl <GHDL bin dir> Path to GHDL's binary directory, e.g. /usr/local/bin"
	echo "  --out <dir name>      Name of the output directory, e.g. xilinx-vivado"
	echo "  --src <Path to lib>   Path to the sources, e.g. /opt/altera/16.0/quartus/eda/sim_lib"
	echo ""
	echo "Verbosity:"
	echo "  -n --no-warnings      Suppress all warnings. Show only error messages."
	echo ""
	exit 0
fi

if [ "$COMPILE_ALL" == "TRUE" ]; then
	COMPILE_ALTERA=TRUE
	COMPILE_MAX=TRUE
	COMPILE_CYCLONE=TRUE
	COMPILE_ARRIA=TRUE
	COMPILE_STRATIX=TRUE
	COMPILE_NM=TRUE
fi

if [ $VHDLStandard -eq 2008 ]; then
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

# -> $SourceDirectories
# -> $DestinationDirectories
# -> $SrcDir
# -> $DestDir
# -> $GHDLBinDir
# <= $SourceDirectory
# <= $DestinationDirectory
# <= $GHDLBinary
SetupDirectories AlteraQuartus "Altera Quartus"

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

# define global GHDL Options
GHDL_OPTIONS=(-fexplicit -frelaxed-rules --no-vital-checks --warn-binding --mb-comments)


GHDL_PARAMS=(${GHDL_OPTIONS[@]})
GHDL_PARAMS+=(--ieee=$VHDLFlavor --std=$VHDLStandard -P$DestinationDirectory)

STOPCOMPILING=0
ERRORCOUNT=0

# Cleanup directories
# ==============================================================================
if [ "$CLEAN" == "TRUE" ]; then
	echo 1>&2 -e "${COLORED_ERROR} '--clean' is not implemented!"
	exit -1
	echo -e "${ANSI_YELLOW}Cleaning up vendor directory ...${ANSI_NOCOLOR}"
	rm *.o 2> /dev/null
	rm *.cf 2> /dev/null
fi


# Altera standard libraries
# ==============================================================================
# compile lpm library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ALTERA" == "TRUE" ]; then
	Library="lpm"
	Files=(
		220pack.vhd
		220model.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile sgate library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ALTERA" == "TRUE" ]; then
	Library="sgate"
	Files=(
		sgate_pack.vhd
		sgate.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile altera library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ALTERA" == "TRUE" ]; then
	Library="altera"
	Files=(
		altera_europa_support_lib.vhd
		altera_primitives_components.vhd
		altera_primitives.vhd
		altera_standard_functions.vhd
		altera_syn_attributes.vhd
		alt_dspbuilder_package.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile altera_mf library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ALTERA" == "TRUE" ]; then
	Library="altera_mf"
	Files=(
		altera_mf_components.vhd
		altera_mf.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
fi

# compile altera_lnsim library
if [ $STOPCOMPILING -eq 0 ] && [ "$COMPILE_ALTERA" == "TRUE" ]; then
	Library="altera_lnsim"
	Files=(
		altera_lnsim_components.vhd
	)
	# append absolute source path
	SourceFiles=()
	for File in ${Files[@]}; do
		SourceFiles+=("$SourceDirectory/$File")
	done

	GHDLCompilePackages
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

echo "--------------------------------------------------------------------------------"
echo -n "Compiling Altera Quartus libraries "
if [ $ERRORCOUNT -gt 0 ]; then
	echo -e $COLORED_FAILED
else
	echo -e $COLORED_SUCCESSFUL
fi
