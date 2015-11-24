#! /bin/bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Bash Script:				Script to compile the simulation libraries from Xilinx ISE
#											for GHDL on Windows
# 
#	Authors:						Patrick Lehmann
# 
# Description:
# ------------------------------------
#	This is a PowerShell script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Xilinx ISE simulation libraries and packages
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


# source configuration file from GHDL's 'vendors' library directory
source config.sh


# extract data from configuration
DestinationDir="$XilinxDestinationDirectory"
SourceDir="$XilinxISEDirectory/ISE_DS/ISE/vhdl/src"
echo $SourceDir
ScriptDir=".."


# define global GHDL Options


# define color escape codes
RED='\e[0;31m'			# Red
GREEN='\e[0;32m'		# Red
YELLOW='\e[1;33m'		# Yellow
CYAN='\e[1;36m'			# Cyan
NOCOLOR='\e[0m'			# No Color



# create "Xilinx" directory and change to it
if [[ -d "$DestinationDir" ]]; then
	echo -e "${YELLOW}Vendor directory '$DestinationDir' already exists."
else
	echo -e "${YELLOW}Creating vendor directory: '$DestinationDir'"
	mkdir "$DestinationDir"
fi
cd $DestinationDir

Clean=0
Unisim=1
Unimacro=0
Simprim=0

SkipExistingFiles=0
SuppressWarnings=1
StopCompiling=1

if [ $SuppressWarnings -eq 0 ]; then
	GRCRulesFile="$ScriptDir/ghdl.grcrules"
else
	GRCRulesFile="$ScriptDir/ghdl.skipwarning.grcrules"
fi


# Cleanup directory
# ==============================================================================
if [[ $Clean -eq 1 ]]; then
	echo -e "${YELLOW}Cleaning up vendor directory ..."
	rm *.o
fi

# Library UNISIM
# ==============================================================================
# compile unisim packages
if [ $Unisim -eq 1 ]; then
	echo -e "${YELLOW}Compiling library 'unisim' ...${NOCOLOR}"
	Files=(
		$SourceDir/unisims/unisim_VPKG.vhd
		$SourceDir/unisims/unisim_VCOMP.vhd
	)

	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [[ ($SkipExistingFiles -eq 1) && (-e "${FileName%.*}.o") ]]; then
			echo -n ""
#			echo -e "${CYAN}Skipping package '$File'${NOCOLOR}"
		else
			echo -e "${CYAN}Analyzing package '$File'${NOCOLOR}"
			ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unisim $File 2>&1 | grcat $GRCRulesFile
		fi
	done
fi

# compile unisim primitives
if [ $Unisim -eq 1 ]; then
	Files=$SourceDir/unisims/primitive/*.vhd
	for File in $Files; do
		FileName=$(basename "$File")
		if [[ ($SkipExistingFiles -eq 1) && (-e "${FileName%.*}.o") ]]; then
			echo -n ""
#			echo -e "${CYAN}Skipping package '$File'${NOCOLOR}"
		else
			echo -e "${CYAN}Analyzing primitive '$File'${NOCOLOR}"
			ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unisim $File 2>&1 | grcat $GRCRulesFile
		fi
	done
fi

# compile unisim secureip primitives
if [ $Unisim -eq 1 ]; then
	echo -e "${YELLOW}Compiling library secureip primitives${NOCOLOR}"
	Files=$SourceDir/unisims/secureip/*.vhd
	for File in $Files; do
		FileName=$(basename "$File")
		if [[ ($SkipExistingFiles -eq 1) && (-e "${FileName%.*}.o") ]]; then
			echo -n ""
#			echo -e "${CYAN}Skipping package '$File'${NOCOLOR}"
		else
			echo -e "${CYAN}Analyzing primitive '$File'${NOCOLOR}"
			ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=secureip $File 2>&1 | grcat $GRCRulesFiles
		fi
	done
fi

# Library UNIMACRO
# ==============================================================================
# compile unimacro packages
if [ $Unimacro -eq 1 ]; then
	echo -e "${YELLOW}Compiling library 'unimacro' ...${NOCOLOR}"

	Files=(
		$SourceDir/unimacro/unimacro_VCOMP.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [[ ($SkipExistingFiles -eq 1) && (-e "${FileName%.*}.o") ]]; then
			echo -n ""
#			echo -e "${CYAN}Skipping package '$File'${NOCOLOR}"
		else
			echo -e "${CYAN}Analyzing package '$File'${NOCOLOR}"
			ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unimacro $File 2>&1 | grcat $GRCRulesFiles
		fi
	done
fi
	
# compile unimacro macros
if [ $Unimacro -eq 1 ]; then
	Files=$SourceDir/unimacro/*_MACRO.vhd*
	for File in $Files; do
		FileName=$(basename "$File")
		if [[ ($SkipExistingFiles -eq 1) && (-e "${FileName%.*}.o") ]]; then
			echo -n ""
#			echo -e "${CYAN}Skipping package '$File'${NOCOLOR}"
		else
			echo -e "${CYAN}Analyzing primitive '$File'${NOCOLOR}"
			ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unisim $File 2>&1 | grcat $GRCRulesFiles
		fi
	done
fi

# Library SIMPRIM
# ==============================================================================
# compile simprim packages
if [ $Simprim -eq 1 ]; then
	echo -e "${YELLOW}Compiling library 'simprim' ...${NOCOLOR}"

	Files=(
		$SourceDir/simprims/simprim_Vpackage.vhd
		$SourceDir/simprims/simprim_Vcomponents.vhd
	)
	for File in ${Files[@]}; do
		FileName=$(basename "$File")
		if [[ ($SkipExistingFiles -eq 1) && (-e "${FileName%.*}.o") ]]; then
			echo -n ""
#			echo -e "${CYAN}Skipping package '$File'${NOCOLOR}"
		else
			echo -e "${CYAN}Analyzing package '$File'${NOCOLOR}"
			ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --ieee=synopsys --std=93c --work=simprim $File 2>&1 | grcat $GRCRulesFiles
		fi
	done
fi

# compile unisim primitives
if [ $Simprim -eq 1 ]; then
	Files=$SourceDir/simprims/primitive/other/*.vhd*
	for File in $Files; do
		FileName=$(basename "$File")
		if [[ ($SkipExistingFiles -eq 1) && (-e "${FileName%.*}.o") ]]; then
			echo -n ""
#			echo -e "${CYAN}Skipping package '$File'${NOCOLOR}"
		else
			echo -e "${CYAN}Analyzing primitive '$File'${NOCOLOR}"
			ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --ieee=synopsys --std=93c --work=simprim $File 2>&1 | grcat $GRCRulesFiles
		fi
	done
fi

# compile unisim secureip primitives
if [ $Simprim -eq 1 ]; then
	Files=$SourceDir/simprims/secureip/other/*.vhd*
	for File in $Files; do
		FileName=$(basename "$File")
		if [[ ($SkipExistingFiles -eq 1) && (-e "${FileName%.*}.o") ]]; then
			echo -n ""
#			echo -e "${CYAN}Skipping package '$File'${NOCOLOR}"
		else
			echo -e "${CYAN}Analyzing primitive '$File'${NOCOLOR}"
			ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --ieee=synopsys --std=93c --work=simprim $File 2>&1 | grcat $GRCRulesFiles
		fi
	done
fi
	
echo "--------------------------------------------------------------------------------"
echo -n "Compiling Xilinx ISE libraries "
if [ $StopCompiling -eq 1 ]; then
	echo -e "${RED}[FAILED]${NOCOLOR}"
else
	echo -e "${GREEN}[SUCCESSFUL]${NOCOLOR}"
fi

cd $WorkingDir