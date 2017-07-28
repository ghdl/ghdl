# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	PowerShell Script:	Script to compile the OSVVM library for GHDL on Windows
# 
# Description:
# ------------------------------------
#	This is a PowerShell script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all OSVVM packages 
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

# .SYNOPSIS
# This CmdLet compiles the OSVVM library.
# 
# .DESCRIPTION
# This CmdLet:
#   (1) creates a subdirectory in the current working directory
#   (2) compiles all OSVVM packages
# 
[CmdletBinding()]
param(
	# Show the embedded help page(s)
	[switch]$Help =							$false,
	
	# Compile all libraries and packages.
	[switch]$All =							$false,
	
	# Compile all OSVVM packages.
	[switch]$OSVVM =						$false,
	
	# Clean up directory before analyzing.
	[switch]$Clean =						$false,
	
	# Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false,
	# Halt on errors
	[switch]$HaltOnError =			$false,
	
	# Set vendor library source directory.
	[string]$Source =						"",
	# Set output directory name.
	[string]$Output =						"",
	# Set GHDL binary directory.
	[string]$GHDL =							""
)

# ---------------------------------------------
# save working directory
$WorkingDir =		Get-Location

# set default values
$EnableDebug =		[bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
$EnableVerbose =	[bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug

# load modules from GHDL's 'vendors' library directory
Import-Module $PSScriptRoot\config.psm1 -Verbose:$false -Debug:$false -ArgumentList "OSVVM"
Import-Module $PSScriptRoot\shared.psm1 -Verbose:$false -Debug:$false -ArgumentList @("OSVVM", "$WorkingDir")

# Display help if no command was selected
if ($Help -or (-not ($All -or $OSVVM -or $Clean)))
{	Get-Help $MYINVOCATION.InvocationName -Detailed
	Exit-CompileScript
}

if ($All)
{	$OSVVM =			$true
}

				
$SourceDirectory =			Get-SourceDirectory $Source ""
$DestinationDirectory =	Get-DestinationDirectory $Output
$GHDLBinary =						Get-GHDLBinary $GHDL

# create "Altera" directory and change to it
New-DestinationDirectory $DestinationDirectory
cd $DestinationDirectory


$VHDLVersion,$VHDLStandard,$VHDLFlavor = Get-VHDLVariables

# define global GHDL Options
$GHDLOptions = @("-a", "-fexplicit", "-frelaxed-rules", "--mb-comments", "--warn-binding", "--ieee=$VHDLFlavor", "--no-vital-checks", "--std=$VHDLStandard", "-P$DestinationDirectory")

# extract data from configuration
# $SourceDir =			$InstallationDirectory["AlteraQuartus"] + "\quartus\eda\sim_lib"

$ErrorCount =			0

# Cleanup directories
# ==============================================================================
if ($Clean)
{	Write-Host "[ERROR]: '-Clean' is not implemented!" -ForegroundColor Red
	Exit-CompileScript -1
	
	Write-Host "Cleaning up vendor directory ..." -ForegroundColor Yellow
	rm *.cf
}


# OSVVM packages
# ==============================================================================
# compile osvvm library
if ((-not $StopCompiling) -and $OSVVM)
{	$Library = "osvvm"
	$Files = @(
		"NamePkg.vhd",
		"OsvvmGlobalPkg.vhd",
		"VendorCovApiPkg.vhd",
		"TranscriptPkg.vhd",
		"TextUtilPkg.vhd",
		"AlertLogPkg.vhd",
		"MessagePkg.vhd",
		"SortListPkg_int.vhd",
		"RandomBasePkg.vhd",
		"RandomPkg.vhd",
		"CoveragePkg.vhd",
		"MemoryPkg.vhd",
		"ScoreboardGenericPkg.vhd",
		"ScoreboardPkg_slv.vhd",
		"ScoreboardPkg_int.vhd",
		"ResolutionPkg.vhd",
		"TbUtilPkg.vhd",
		"OsvvmContext.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling OSVVM packages " -NoNewline
if ($ErrorCount -gt 0)
{	Write-Host "[FAILED]" -ForegroundColor Red				}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green	}

Exit-CompileScript
