# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	PowerShell Script:	Script to compile GHDL for Windows
# 
#	Authors:						Patrick Lehmann	(ported batch file to PowerShell)
#											Brian Davis			(contributions to the batch file)
#											Tristan Gingold	(initial batch file for compilations on Windows)
# 
# Description:
# ------------------------------------
#	This is a PowerShell script (executable) which:
#		- sets up a compilation environment
#		- test all dependencies
#		- compiles GHDL with GNAT
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
<#
	.SYNOPSIS 
	GHDL for Windows - GHDL compile script
	Use 'compile.ps1 -Help' to see the integrated help page
	
	.EXAMPLE
	C:\PS> .\compile.ps1 -Clean -Compile
#>

# define script parameters
[CmdletBinding()]
Param(
	# compile ALL
	[switch]$All =			$false,
	
	# compile main targets
	[switch]$Compile =	$false,
		# compile GHDL (simulator)
		[switch]$GHDL =		$false,
		[switch]$Test =		$false,
	
	# compile TOOLS
	[switch]$Tools =		$false,
		# compile Filter (helper)
		[switch]$Filter =	$false,
	
	# build options
	[switch]$Release =	$false,
	
	# clean up all files and directories
	[switch]$Clean =		$false,
	
	# display this help"
	[switch]$Help =			$false
)

# configure script here
$Script_RelPathToRoot =	"..\..\.."

# save parameters and current working directory
$Script_Parameters =	$args
$Script_WorkingDir =	Get-Location
$GHDLRootDir =				Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $Script_RelPathToRoot))

# set default values
$Script_ExitCode = 		0
$BuildRelease =				"Development"		# "Release"

if ($All)
{	$Compile =	$true
	$Tools =		$true
}
if ($Compile)
{	$GHDL =			$true
	$Test =			$true
}
if ($Tools)
{	$Filter =		$true
}

if ($Release)
{	$BuildRelease =		"Release"			}
else
{	$BuildRelease =		"Development"	}

$NoCommand = -not ($Clean -or $All -or $Compile -or $Tools -or $GHDL -or $Test -or $Filter)
if ($NoCommand)
{	$Help = $true		}

Write-Host "================================================================================" -ForegroundColor Magenta
Write-Host "GHDL for Windows - GHDL and tools compile script" -ForegroundColor Magenta
Write-Host "================================================================================" -ForegroundColor Magenta

# if command is help or no command was given => display help page(s)
if ($Help)
{	Write-Host "Usage:"
	Write-Host "  compile.ps1 (-Help|-Clean|-All|-Compile|-Tools|-GHDL|-Test|-Filter)" -ForegroundColor Gray
	Write-Host
	Write-Host "Options:"
	Write-Host "  -Release    build in release mode"
	# Write-Host "  -Debug      enable debug messages"
	# Write-Host
	Write-Host "Commands:"
	Write-Host "  -Help       display this help"
	Write-Host "  -All        compile all targets"
	Write-Host "  -Compile    compile all main targets"
	Write-Host "  -Tools      compile all tool targets"
	Write-Host "  -GHDL       compile ghdl.exe"
	Write-Host "  -Filter     compile filter.exe"
	Write-Host "  -Clean      clean up all files and directories"
	Write-Host
	
	exit 0
}	# Help

# load modules
Import-Module $PSScriptRoot\shared.psm1
Import-Module $PSScriptRoot\targets.psm1

# grep GHDL version string from Ada source file
$GHDLVersion = 				Get-GHDLVersion $GHDLRootDir

# gather git information
$Git_IsGitRepo =						Test-GitRepository
if ($Git_IsGitRepo)
{	$Git_Branch_Name =				& git rev-parse --abbrev-ref HEAD
	$Git_Commit_DataString =	& git log -1 --format=%cd --date=short
	$Git_Commit_ShortHash =		& git rev-parse --short HEAD
}

Write-Host "  Version:    $GHDLVersion"
Write-Host "  Release:    $BuildRelease"
if ($Git_IsGitRepo)
{	Write-Host "  Git branch: $Git_Branch_Name"
	Write-Host "  Git commit: $Git_Commit_DataString ($Git_Commit_ShortHash)"
}
Write-Host

function Write-TargetResult($error)
{	if ($error)
	{	Write-Host "  [FAILED]"	-ForegroundColor Red		}
	# else
	# {	Write-Host "  [DONE]"		-ForegroundColor Green	}
}

if ($BuildRelease -eq "Release")
{	$BuildDir =		$GHDLRootDir + "\dist\mcode\build"		}
elseif ($BuildRelease -eq "Development")
{	$BuildDir =		$GHDLRootDir + "\dist\mcode\build"		}
else
{	Write-Host "[ERROR]: Unknown build setting '$BuildRelease'." -ForegroundColor Red
	exit 1
}

# ==============================================================================
# Main Target: Clean
# ==============================================================================
if ($Clean)
{	$error = Invoke-Clean $BuildDir
	Write-TargetResult $error
}	# Clean


# ==============================================================================
# Main Target: GHDL
# ==============================================================================
if ($GHDL)
{	# create a build directory
	$error = Invoke-CreateBuildDirectory $BuildDir
	Write-TargetResult $error
	
	# patch the version file if it's no release build
	if ((-not $error) -and ($BuildRelease -eq "Development") -and $Git_IsGitRepo)
	{	$error = Invoke-PatchVersionFile $GHDLRootDir $Git_Branch_Name $Git_Commit_DataString $Git_Commit_ShortHash
		Write-TargetResult $error
	}
	
	# build C source files
	if (-not $error)
	{	$error = Invoke-CompileCFiles $GHDLRootDir $BuildDir
		Write-TargetResult $error
	}
	
	# build Ada source files
	if (-not $error)
	{	$error = Invoke-CompileGHDLAdaFiles $GHDLRootDir $BuildDir
		Write-TargetResult $error
	}
	
	# strip result
	if (-not $error)
	{	$error = Invoke-StripGHDLExecutable $BuildDir
		Write-TargetResult $error
	}
	
	# restore the version file if it was patched
	if ((-not $error) -and ($BuildRelease -eq "Development") -and $Git_IsGitRepo)
	{	$error = Restore-PatchedVersionFile $GHDLRootDir
		Write-TargetResult $error
	}
} # Compile

if ($Test)
{	# running ghdl
	$error = Test-GHDLVersion $BuildDir
	Write-TargetResult $error
}	# Test

# ==============================================================================
# Tool Target: Filter
# ==============================================================================
if ($Filter)
{	# create a build directory
	$error = Invoke-CreateBuildDirectory $BuildDir
	Write-TargetResult $error

	# build Ada source files
	if (-not $error)
	{	$error = Invoke-CompileFilterAdaFiles $GHDLRootDir $BuildDir
		Write-TargetResult $error
	}
}	# Tools


# unload PowerShell modules
Remove-Module shared
Remove-Module targets
	
# restore working directory if changed
Set-Location $Script_WorkingDir

# return exit status
exit $Script_ExitCode
