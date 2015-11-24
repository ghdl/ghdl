# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	PowerShell Module:	Configurable directories to local installed tools
# 
#	Authors:						Patrick Lehmann
# 
# Description:
# ------------------------------------
#	This PowerShell module exports variables containing the users local tool
#	environment.
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
#
# Configure your tools here. Use absolute paths, without trailing directory
#	delimiter. Empty strings indicate not installed tools
$InstallationDirectory = @{
	"AlteraQuartusII" =	"C:\Altera\15.0";
	"XilinxISE" =				"C:\Xilinx\14.7";
	"XilinxVivado" =		"C:\Xilinx\Vivado\2015.3";
	"OSVVM" =						"D:\git\PoC\lib\osvvm";
	"VUnit" =						"D:\git\PoC\lib\vunit"
}

$DestinationDirectory = @{
	"Altera" =					"altera";
	"XilinxISE" =				"xilinx";
	"XilinxVivado" =		"vivado";
	"OSVVM" =						"osvvm";
	"VUnit" =						"vunit"
}

Export-ModuleMember -Variable 'InstallationDirectory'
Export-ModuleMember -Variable 'DestinationDirectory'
