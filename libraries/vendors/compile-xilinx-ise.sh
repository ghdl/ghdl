#! /bin/bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	PowerShell Script:	Script to compile the simulation libraries from Xilinx ISE
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
#	Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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


# source configuration file from GHDL's 'vendors' library directory
source config.sh


# extract data from configuration
DestinationDir="$XilinxDestinationDirectory"
SourceDir="$XilinxISEDirectory/ISE_DS/ISE/vhdl/src"
echo $SourceDir


# define global GHDL Options


# create "Xilinx" directory and change to it
Write-Host "Creating vendor directory: '$DestinationDir'"
mkdir $DestinationDir
cd $DestinationDir


# Library UNISIM
# ==============================================================================
# compile unisim packages
echo "Compiling library 'unisim' ..."

Files=(
	$SourceDir/unisims/unisim_VPKG.vhd
	$SourceDir/unisims/unisim_VCOMP.vhd
)

for File in ${Files[@]}; do
	echo "Analysing package '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unisim $File
done

# compile unisim primitives
Files=$SourceDir/unisims/primitive/*.vhd
for File in $Files; do
	echo "Analysing primitive '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unisim $File
done

# compile unisim secureip primitives
echo "Compiling library secureip primitives"
Files=$SourceDir/unisims/secureip/*.vhd
for File in $Files; do
	echo "Analysing primitive '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=secureip $File
done


# Library UNIMACRO
# ==============================================================================
# compile unimacro packages
echo "Compiling library 'unimacro' ..."

Files=(
	$SourceDir/unimacro/unimacro_VCOMP.vhd
)

for File in ${Files[@]}; do
	echo "Analysing package '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unimacro $File
done

# compile unimacro macros
Files=$SourceDir/unimacro/*_MACRO.vhd*
for File in $Files; do
	echo "Analysing primitive '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --no-vital-checks --ieee=synopsys --std=93c --work=unisim $File
done


# Library SIMPRIM
# ==============================================================================
# compile simprim packages
echo "Compiling library 'simprim' ..."

Files=(
	$SourceDir/simprim/simprim_Vpackage.vhd
	$SourceDir/simprim/simprim_Vcomponents.vhd
)

for File in ${Files[@]}; do
	echo "Analysing package '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --ieee=synopsys --std=93c --work=simprim $File
done

# compile unisim primitives
Files=$SourceDir/simprim/primitive/other/*.vhd*
for File in $Files; do
	echo "Analysing primitive '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --ieee=synopsys --std=93c --work=simprim $File
done

# compile unisim secureip primitives
Files=$SourceDir/simprim/secureip/other/*.vhd*
for File in $Files; do
	echo "Analysing primitive '$File'"
	ghdl -a -fexplicit -frelaxed-rules --warn-binding --mb-comments --ieee=synopsys --std=93c --work=simprim $File
done

echo "--------------------------------------------------------------------------------"
echo "Compiling Xilinx ISE libraries "
echo "[FAILED]"
echo "[SUCCESSFUL]"

cd ..