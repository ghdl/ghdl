#! /bin/bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	Bash Script:				Configurable directories to local installed tools
# 
# Description:
# ------------------------------------
#	This Bash file exports variables containing the users local tool environment.
#
# ==============================================================================
#	Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
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


# Configure
# - vendor tool chain installation paths or
# - library root directories
# in the following dictionary.
# 
# These values are used if no command line argument (--src) is passed to a
# compile script. Empty strings means not configured.
declare -A InstallationDirectories
InstallationDirectories[AlteraQuartus]=""     # "/opt/altera/16.0/quartus"
InstallationDirectories[IntelQuartus]=""      # "/opt/intelFPGA/17.0/quartus"
InstallationDirectories[LatticeDiamond]=""    # "/usr/local/diamond/3.7_x64"
InstallationDirectories[OSVVM]=""	   					# "~/git/github/osvvm"
InstallationDirectories[UVVM]=""	   					# "~/git/github/uvvm_all"
InstallationDirectories[VUnit]=""	   					# "~/git/github/vunit"
InstallationDirectories[XilinxISE]=""	  		  # "/opt/Xilinx/14.7/ISE_DS/ISE"
InstallationDirectories[XilinxVivado]=""      # "/opt/Xilinx/Vivado/2016.2"

# Configure preferred output directories for each library set:
declare -A DestinationDirectories
DestinationDirectories[AlteraQuartus]="altera"
DestinationDirectories[IntelQuartus]="intel"
DestinationDirectories[LatticeDiamond]="lattice"
DestinationDirectories[OSVVM]="."										# "osvvm"
DestinationDirectories[UVVM]="."
DestinationDirectories[VUnit]="."										# "vunit_lib"
DestinationDirectories[XilinxISE]="xilinx-ise"
DestinationDirectories[XilinxVivado]="xilinx-vivado"

# Declare source directories depending on the installation paths:
declare -A SourceDirectories
SourceDirectories[AlteraQuartus]="eda/sim_lib"
SourceDirectories[IntelQuartus]="eda/sim_lib"
SourceDirectories[LatticeDiamond]="cae_library/simulation/vhdl"
SourceDirectories[OSVVM]="."
SourceDirectories[UVVM]="."
SourceDirectories[VUnit]="vunit/vhdl"
SourceDirectories[XilinxISE]="vhdl/src"
SourceDirectories[XilinxVivado]="data/vhdl/src"

# input files greater than $LARGE_FILESIZE are skipped if '--skip-largefiles' is set
LARGE_FILESIZE=125000
