# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	PowerShell Module:	Configurable directories to local installed tools
# 
# Description:
# ------------------------------------
#	This PowerShell module exports variables containing the users local tool
#	environment.
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
#
[CmdletBinding()]
param(
	[Parameter(Mandatory=$true)][string]$VendorToolName
)

$Module_VendorToolName = $VendorToolName

# Configure your tools here. Use absolute paths, without trailing directory
#	delimiter. Empty strings indicate not installed tools
$InstallationDirectories = @{
	"AlteraQuartus" =		""		# "C:\Altera\16.0\quartus";
	"LatticeDiamond" =	""		# "C:\Lattice\Diamond\3.8_x64"
	"UVVM" =						""		# "C:\git\GitHub\uvvm_all";
	"XilinxISE" =				""		# "C:\Xilinx\14.7\ISE_DS";
	"XilinxVivado" =		""		# "C:\Xilinx\Vivado\2016.3";
	"OSVVM" =						""		# "C:\git\GitHub\osvvm";
	"VUnit" =						""		# "C:\git\GitHub\vunit"
}

$SourceDirectories = @{
	"AlteraQuartus" =		"eda\sim_lib";
	"LatticeDiamond" =	"cae_library\simulation\vhdl"
	"UVVM" =						".";
	"XilinxISE" =				"ISE\vhdl\src";
	"XilinxVivado" =		"data\vhdl\src";
	"OSVVM" =						".";
	"VUnit" =						"vunit\vhdl"
}

$DestinationDirectories = @{
	"AlteraQuartus" =		"altera";
	"LatticeDiamond" =	"lattice";
	"UVVM" =						".";
	"XilinxISE" =				"xilinx-ise";
	"XilinxVivado" =		"xilinx-vivado";
	"OSVVM" =						".";
	"VUnit" =						"."
}


function Get-VendorToolInstallationDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
	#>
	return $InstallationDirectories[$Module_VendorToolName]
}

function Get-VendorToolSourceDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
	#>
	return $SourceDirectories[$Module_VendorToolName]
}

function Get-VendorToolDestinationDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
	#>
	return $DestinationDirectories[$Module_VendorToolName]
}

Export-ModuleMember -Function 'Get-VendorToolInstallationDirectory'
Export-ModuleMember -Function 'Get-VendorToolSourceDirectory'
Export-ModuleMember -Function 'Get-VendorToolDestinationDirectory'
