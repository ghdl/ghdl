# EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
#
# ==============================================================================
#  Authors:
#    Patrick Lehmann  (ported batch file to PowerShell)
#    Brian Davis      (contributions to the batch file)
#    Tristan Gingold  (initial batch file for compilations on Windows)
#
#  PowerShell Script: Script to compile GHDL for Windows
#
# Description:
# ------------------------------------
#  This is a PowerShell script (executable) which:
#    - sets up a compilation environment
#    - test all dependencies
#    - compiles GHDL with GNAT
#
# ==============================================================================
#  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
#  Copyright (C) 2015-2017 Patrick Lehmann
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
# GHDL for Windows - GHDL compile script
# Use 'compile-ghdl.ps1 -Help' to see the integrated help page
# 
# .EXAMPLE
# C:\PS> .\compile-ghdl.ps1 -Clean -Compile
# 
[CmdletBinding()]
param(
	# Display this help"
	[switch]$Help =							$false,
	
	# Slean up all files and directories
	[switch]$Clean =						$false,
		[switch]$Clean_GHDL =			$false,
	
	# Compile all targets
	[switch]$All =							$false,
	
	# Compile main targets
	[switch]$Compile =					$false,
		# Compile GHDL (simulator)
		[switch]$Compile_GHDL =		$false,
	# Undocumented
	[switch]$Test =							$false,
		# Undocumented
		[switch]$Test_GHDL =			$false,
	
	# Build options
	# Build a release version
	[switch]$Release =					$false,
	# Set the back-end
	[string]$Backend =					"mcode",
	
	# Reduced messages
	[switch]$Quiet =						$false,
	# Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false,
	# Halt on errors
	[switch]$HaltOnError =			$false,
	# Undocumented
	[switch]$Hosted =						$false
)

# configure script here
$RelPathToRoot =			"..\.."

# ---------------------------------------------
# save parameters and working directory
$Script_ScriptDir =		$PSScriptRoot
$Script_WorkingDir =	Get-Location
$GHDLRootDir =				Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $RelPathToRoot))

# set default values
$EnableDebug =        [bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
$EnableVerbose =      [bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug

# load modules from GHDL's 'libraries' directory
Import-Module $PSScriptRoot\shared.psm1  -Verbose:$false -Debug:$false -ArgumentList "$Script_WorkingDir", $Hosted
Import-Module $PSScriptRoot\targets.psm1 -Verbose:$false -Debug:$false

# Display help if no command was selected
$Help = $Help -or (-not (
					$All -or 
					$Clean -or $Clean_GHDL -or
					$Compile -or $Compile_GHDL -or
					$Test -or $Test_GHDL
				))

if (-not $Hosted)
{	Write-Host "================================================================================" -ForegroundColor Magenta
	Write-Host "GHDL for Windows - GHDL compile script" -ForegroundColor Magenta
	Write-Host "================================================================================" -ForegroundColor Magenta
}

if ($Help)
{	Get-Help $MYINVOCATION.MyCommand.Path -Detailed
	Exit-CompileScript
}

if ($Clean)
{	$Clean_GHDL =		$true
}
if ($All)
{	$Compile =			$true
}
if ($Compile)
{	$Compile_GHDL =	$true
}
if ($Test)
{	$Test_GHDL =		$true
}

# configure some variables: paths, executables, directory names, ...
$BuildDirectoryName =						"build"

# Parameter checks
if ($Backend -ne "mcode")
{	Write-Host "[ERROR]: Back-end '$Backend' is not supported on Windows." -ForegroundColor Red
	Exit-CompileScript -1
}

# construct directories
$BinaryDestinationDirectory =		"$GHDLRootDir\$BuildDirectoryName\$Backend"
# construct executables
$GHDLNewExecutable =						"$GHDLRootDir\$BuildDirectoryName\$Backend\bin\ghdl.exe"

# grep GHDL version string from Ada source file
$GHDLVersion = 							Get-GHDLVersion $GHDLRootDir
# compute some variables
$BuildRelease = if ($Release)	{	"Release"	}	else	{	"Development"	}
if (-not $Hosted)
{	Write-Host "  Version:    $GHDLVersion"
	Write-Host "  Release:    $BuildRelease"
}

$Git_IsGitRepo =						Test-GitRepository
# gather git information
if ($Git_IsGitRepo)
{	$Git_Branch_Name =				& git rev-parse --abbrev-ref HEAD
	$Git_Commit_DateString =	& git log -1 --format=%cd --date=short
	$Git_Commit_ShortHash =		& git rev-parse --short HEAD

	if (-not $Hosted)
	{	Write-Host "  Git branch: $Git_Branch_Name"
		Write-Host "  Git commit: $Git_Commit_DataString ($Git_Commit_ShortHash)"
	}
}
if (-not $Hosted)
{	Write-Host ""	}

if ($Release)
{	$BuildDirectory =		$BinaryDestinationDirectory		}
else
{	$BuildDirectory =		$BinaryDestinationDirectory		}


# ==============================================================================
# Main Target: Clean
# ==============================================================================
if ($Clean_GHDL)
{	$error = Invoke-Clean $BuildDirectory -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug
	if ($error -eq $true)
	{	Write-Host "  [FAILED]"	-ForegroundColor Red
		Exit-CompileScript -1
	}
}	# Clean


# ==============================================================================
# Main Target: GHDL
# ==============================================================================
if ($Compile_GHDL)
{	# create a build directory
	$error = New-BuildDirectory $BuildDirectory
	if ($error -eq $true)
	{	Write-Host "  [FAILED]"	-ForegroundColor Red
		Exit-CompileScript -1
	}
	
	# patch the version file if it's no release build
	if (-not $Release -and $Git_IsGitRepo)
	{	$error = Invoke-PatchVersionFile $GHDLRootDir $Git_Branch_Name $Git_Commit_DateString $Git_Commit_ShortHash
		if ($error -eq $true)
		{	Write-Host "  [FAILED]"	-ForegroundColor Red
			Exit-CompileScript -1
		}
	}
	
	# build C source files
	$error = Invoke-CompileCFiles $GHDLRootDir $BinaryDestinationDirectory
	if ($error -eq $true)
	{	Write-Host "  [FAILED]"	-ForegroundColor Red
		Exit-CompileScript -1
	}
	
	# build Ada source files
	$error = Invoke-CompileGHDLAdaFiles $GHDLRootDir $BinaryDestinationDirectory
	if ($error -eq $true)
	{	Write-Host "  [FAILED]"	-ForegroundColor Red
		Exit-CompileScript -1
	}
	
	# strip result
	$error = Invoke-StripGHDLExecutable $BinaryDestinationDirectory
	if ($error -eq $true)
	{	Write-Host "  [FAILED]"	-ForegroundColor Red
		Exit-CompileScript -1
	}
}


# ==============================================================================
# Main Target: GHDL
# ==============================================================================
if ($Test_GHDL)
{	# running ghdl
	$error = Test-GHDLVersion $BuildDir
	if ($error -eq $true)
	{	Write-Host "  [FAILED]"	-ForegroundColor Red
		Exit-CompileScript -1
	}
}	# Test

Exit-CompileScript 0
