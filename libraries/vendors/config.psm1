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
#	Copyright (C) 2017-2018 Patrick Lehmann - Freiburg, Germany
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
$Settings = @{
	"AlteraQuartus" =		@{
		"InstallationDirectory" = "";								# "C:\Altera\16.0\quartus";
		"SourceDirectory" =       "eda\sim_lib";
		"DestinationDirectory" =  "altera"
	};
	"IntelQuartus" =		@{
		"InstallationDirectory" = "";								# "C:\IntelFPGA\17.1\quartus";
		"SourceDirectory" =       "eda\sim_lib";
		"DestinationDirectory" =  "intel"
	};
	"LatticeDiamond" =	@{
		"InstallationDirectory" = "";								# "C:\Lattice\Diamond\3.8_x64";
		"SourceDirectory" =       "cae_library\simulation\vhdl";
		"DestinationDirectory" =  "lattice"
	};
	"OSVVM" =						@{
		"InstallationDirectory" = "";								# "C:\git\GitHub\osvvm";
		"SourceDirectory" =       ".";
		"DestinationDirectory" =  "."
	};
	"UVVM" =						@{
		"InstallationDirectory" = "";								# "C:\git\GitHub\uvvm_alls";
		"SourceDirectory" =       ".";
		"DestinationDirectory" =  "."
	};
	"XilinxISE" =				@{
		"InstallationDirectory" = "";								# "C:\Xilinx\14.7\ISE_DS";
		"SourceDirectory" =       "ISE\vhdl\src";
		"DestinationDirectory" =  "xilinx-ise"
	};
	"XilinxVivado" =		@{
		"InstallationDirectory" = "";								# "C:\Xilinx\Vivado\2017.4";
		"SourceDirectory" =       "data\vhdl\src";
		"DestinationDirectory" =  "xilinx-vivado"
	}
}


function Get-VendorToolInstallationDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
	#>
	return $Settings[$Module_VendorToolName]["InstallationDirectory"]
}

function Get-VendorToolSourceDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
	#>
	return $Settings[$Module_VendorToolName]["SourceDirectory"]
}

function Get-VendorToolDestinationDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
	#>
	return $Settings[$Module_VendorToolName]["DestinationDirectory"]
}

Export-ModuleMember -Function 'Get-VendorToolInstallationDirectory'
Export-ModuleMember -Function 'Get-VendorToolSourceDirectory'
Export-ModuleMember -Function 'Get-VendorToolDestinationDirectory'
