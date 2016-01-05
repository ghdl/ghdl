# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	PowerShell Script:	Script to compile the VUnit library for GHDL on Windows
# 
#	Authors:						Patrick Lehmann
# 
# Description:
# ------------------------------------
#	This is a PowerShell script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all VUnit packages 
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
	# Compile all packages.
	[switch]$All =							$true,
	
	# Clean up directory before analyzing.
	[switch]$Clean =						$false,
	
	#Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false,
	# Halt on errors
	[switch]$HaltOnError =			$false,
	
	# Show the embedded help page(s)
	[switch]$Help =							$false
)

if ($Help)
{	Get-Help $MYINVOCATION.InvocationName -Detailed
	return
}

# ---------------------------------------------
# save working directory
$WorkingDir = Get-Location

# load modules from GHDL's 'vendors' library directory
Import-Module $PSScriptRoot\config.psm1
Import-Module $PSScriptRoot\shared.psm1

# extract data from configuration
$SourceDir =			$InstallationDirectory["VUnit"]
$DestinationDir = $DestinationDirectory["VUnit"]

if ($All -eq $true)
{	# nothing to configure
}

$ErrorCount =			0

# define global GHDL Options
$GlobalOptions = ("-a", "-fexplicit", "-frelaxed-rules", "--mb-comments", "--warn-binding", "--no-vital-checks", "--std=08")

# create "vunit" directory and change to it
Write-Host "Creating vendor directory: '$DestinationDir'" -ForegroundColor Yellow
mkdir $DestinationDir -ErrorAction SilentlyContinue | Out-Null
cd $DestinationDir

# Cleanup
# ==============================================================================
if ($Clean)
{	Write-Host "Cleaning up vendor directory ..." -ForegroundColor Yellow
	rm *.cf
}

# compile vunit_lib library
Write-Host "Compiling library 'vunit_lib' ..." -ForegroundColor Yellow
$Options = $GlobalOptions
$Files = (
	"$SourceDir\vunit\vhdl\run\src\stop_api.vhd",
	"$SourceDir\vunit\vhdl\vhdl\src\lib\std\textio.vhd",
	"$SourceDir\vunit\vhdl\vhdl\src\lang\lang.vhd",
	"$SourceDir\vunit\vhdl\com\src\com_types.vhd",
	"$SourceDir\vunit\vhdl\run\src\stop_body_2008.vhd",
	"$SourceDir\vunit\vhdl\com\src\com_api.vhd",
	"$SourceDir\vunit\vhdl\string_ops\src\string_ops.vhd",
	"$SourceDir\vunit\vhdl\path\src\path.vhd",
	"$SourceDir\vunit\vhdl\logging\src\log_types.vhd",
	"$SourceDir\vunit\vhdl\logging\src\log_formatting.vhd",
	"$SourceDir\vunit\vhdl\logging\src\log_special_types200x.vhd",
	"$SourceDir\vunit\vhdl\array\src\array_pkg.vhd",
	"$SourceDir\vunit\vhdl\logging\src\log_base_api.vhd",
	"$SourceDir\vunit\vhdl\logging\src\log_base.vhd",
	"$SourceDir\vunit\vhdl\logging\src\log_api.vhd",
	"$SourceDir\vunit\vhdl\logging\src\log.vhd",
	"$SourceDir\vunit\vhdl\check\src\check_types.vhd",
	"$SourceDir\vunit\vhdl\check\src\check_special_types200x.vhd",
	"$SourceDir\vunit\vhdl\check\src\check_base_api.vhd",
	"$SourceDir\vunit\vhdl\check\src\check_base.vhd",
	"$SourceDir\vunit\vhdl\check\src\check_api.vhd",
	"$SourceDir\vunit\vhdl\check\src\check.vhd",
	"$SourceDir\vunit\vhdl\dictionary\src\dictionary.vhd",
	"$SourceDir\vunit\vhdl\run\src\run_types.vhd",
	"$SourceDir\vunit\vhdl\run\src\run_special_types200x.vhd",
	"$SourceDir\vunit\vhdl\run\src\run_base_api.vhd",
	"$SourceDir\vunit\vhdl\run\src\run_base.vhd",
	"$SourceDir\vunit\vhdl\run\src\run_api.vhd",
	"$SourceDir\vunit\vhdl\run\src\run.vhd",
	"$SourceDir\vunit\vhdl\vunit_run_context.vhd",
	"$SourceDir\vunit\vhdl\vunit_context.vhd",
	"$SourceDir\vunit\vhdl\com\src\com_std_codec_builder.vhd",
	"$SourceDir\vunit\vhdl\com\src\com_debug_codec_builder.vhd",
	"$SourceDir\vunit\vhdl\com\src\com_string.vhd",
	"$SourceDir\vunit\vhdl\com\src\com_codec_api.vhd",
	"$SourceDir\vunit\vhdl\com\src\com_codec.vhd",
	"$SourceDir\vunit\vhdl\com\src\com.vhd",
	"$SourceDir\vunit\vhdl\com\src\com_context.vhd")
foreach ($File in $Files)
{	Write-Host "Analyzing package '$File'" -ForegroundColor Cyan
	$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=vunit_lib " + $File + " 2>&1"
	$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
	if ($LastExitCode -ne 0)
	{	$ErrorCount += 1
		if ($HaltOnError)
		{	break		}
	}
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling VUnit library " -NoNewline
if ($ErrorCount -gt 0)
{	Write-Host "[FAILED]" -ForegroundColor Red				}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green	}

# unload PowerShell modules
Remove-Module shared
Remove-Module config

# restore working directory
cd $WorkingDir

