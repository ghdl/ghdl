#! /bin/bash
# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Bash Script:				Configurable directories to local installed tools
# 
#	Authors:						Patrick Lehmann
# 
# Description:
# ------------------------------------
#	This Bash file exports variables containing the users local tool environment.
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

declare -A InstallationDirectory
InstallationDirectory[AlteraQuartusII]="/opt/Altera/15.0"
InstallationDirectory[XilinxISE]="/opt/Xilinx/14.7"
InstallationDirectory[XilinxVivado]="/opt/Xilinx/Vivado/2015.2"
InstallationDirectory[OSVVM]="/home/paebbels/git/PoC/lib/osvvm"
InstallationDirectory[VUnit]="/home/paebbels/git/PoC/lib/VUnit"

declare -A DestinationDirectory
DestinationDirectory[AlteraQuartusII]="altera"
DestinationDirectory[XilinxISE]="xilinx"
DestinationDirectory[XilinxVivado]="vivado"
DestinationDirectory[OSVVM]="osvvm"
DestinationDirectory[VUnit]="vuint"

# input files greater than $LARGE_FILESIZE are skipped if '--skip-largefiles' is set
LARGE_FILESIZE=125000
