# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	PowerShell Script:	Script to compile the VUnit library for GHDL on Windows
# 
# Description:
# ------------------------------------
#	This is a PowerShell script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all VUnit packages 
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
# This CmdLet compiles the VUnit library.
# 
# .DESCRIPTION
# This CmdLet:
#   (1) creates a subdirectory in the current working directory
#   (2) compiles all VUnit packages
#
[CmdletBinding()]
param(
	# Show the embedded help page(s)
	[switch]$Help =							$false,
	
	# Compile all packages.
	[switch]$All =							$true,
	
	# Compile all VUnit packages.
	[switch]$VUnit =						$true,
	
	# Clean up directory before analyzing.
	[switch]$Clean =						$false,
	
	#Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false,
	# Halt on errors.
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
Import-Module $PSScriptRoot\config.psm1 -Verbose:$false -Debug:$false -ArgumentList "VUnit"
Import-Module $PSScriptRoot\shared.psm1 -Verbose:$false -Debug:$false -ArgumentList @("VUnit", "$WorkingDir")

# Display help if no command was selected
$Help = $Help -or (-not ($All -or $VUnit -or $Clean))

if ($Help)
{	Get-Help $MYINVOCATION.InvocationName -Detailed
	Exit-CompileScript
}
if ($All)
{	$VUnit =			$true
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


# VUnit packages
# ==============================================================================
# compile vunit_lib library
if ((-not $StopCompiling) -and $VUnit)
{	$Library = "vunit_lib"
	$Files = @(
		"run\src\stop_api.vhd",
		"vhdl\src\lib\std\textio.vhd",
		"vhdl\src\lang\lang.vhd",
		"com\src\com_types.vhd",
		"run\src\stop_body_2008.vhd",
		"com\src\com_api.vhd",
		"string_ops\src\string_ops.vhd",
		"path\src\path.vhd",
		"logging\src\log_types.vhd",
		"logging\src\log_formatting.vhd",
		"logging\src\log_special_types200x.vhd",
		"array\src\array_pkg.vhd",
		"logging\src\log_base_api.vhd",
		"logging\src\log_base.vhd",
		"logging\src\log_api.vhd",
		"logging\src\log.vhd",
		"check\src\check_types.vhd",
		"check\src\check_special_types200x.vhd",
		"check\src\check_base_api.vhd",
		"check\src\check_base.vhd",
		"check\src\check_api.vhd",
		"check\src\check.vhd",
		"dictionary\src\dictionary.vhd",
		"run\src\run_types.vhd",
		"run\src\run_special_types200x.vhd",
		"run\src\run_base_api.vhd",
		"run\src\run_base.vhd",
		"run\src\run_api.vhd",
		"run\src\run.vhd",
		"vunit_run_context.vhd",
		"vunit_context.vhd",
		"com\src\com_std_codec_builder.vhd",
		"com\src\com_debug_codec_builder.vhd",
		"com\src\com_string.vhd",
		"com\src\com_codec_api.vhd",
		"com\src\com_codec.vhd",
		"com\src\com.vhd",
		"com\src\com_context.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling VUnit packages " -NoNewline
if ($ErrorCount -gt 0)
{	Write-Host "[FAILED]" -ForegroundColor Red				}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green	}

Exit-CompileScript
