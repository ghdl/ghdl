# ==============================================================================
#  Authors:
#    Patrick Lehmann
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

# .SYNOPSIS
# Configuration files to store settings and installation directories for 3rd party tools..
#
# .DESCRIPTION
# This configuaration file stores settings for pre-compile scripts, so scripts
# can be called with less command line arguments.
#
[CmdletBinding()]
param(
	[Parameter(Mandatory=$true)][string]$VendorToolName
)

$Module_VendorToolName = $VendorToolName

# Configure your tools here. Use absolute paths, without trailing directory
# delimiter. Empty strings indicate not installed tools
$Settings = @{
	"AlteraQuartus" =		@{
		"InstallationDirectory" = "";								# "C:\Altera\16.0\quartus";
		"SourceDirectory" =       "eda\sim_lib";
		"DestinationDirectory" =  "altera"
	};
	"IntelQuartus" =		@{
		"InstallationDirectory" = "";								# "C:\IntelFPGA\20.1\quartus";
		"SourceDirectory" =       "eda\sim_lib";
		"DestinationDirectory" =  "intel"
	};
	"LatticeDiamond" =	@{
		"InstallationDirectory" = "";								# "C:\Lattice\Diamond\3.10_x64";
		"SourceDirectory" =       "cae_library\simulation\vhdl";
		"DestinationDirectory" =  "lattice"
	};
	"OSVVM" =						@{
		"InstallationDirectory" = "";								# "C:\git\GitHub\OSVVM";
		"SourceDirectory" =       ".";
		"DestinationDirectory" =  "."
	};
	"UVVM" =						@{
		"InstallationDirectory" = "";								# "C:\git\GitHub\UVVM";
		"SourceDirectory" =       ".";
		"DestinationDirectory" =  "."
	};
	"XilinxISE" =				@{
		"InstallationDirectory" = "";								# "C:\Xilinx\14.7\ISE_DS";
		"SourceDirectory" =       "ISE\vhdl\src";
		"DestinationDirectory" =  "xilinx-ise"
	};
	"XilinxVivado" =		@{
		"InstallationDirectory" = "";								# "C:\Xilinx\Vivado\2020.2";
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
