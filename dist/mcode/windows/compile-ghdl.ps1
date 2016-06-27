# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann	(ported batch file to PowerShell)
#											Brian Davis			(contributions to the batch file)
#											Tristan Gingold	(initial batch file for compilations on Windows)
# 
#	PowerShell Script:	Script to compile GHDL for Windows
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
#	Copyright (C) 2015-2016 Patrick Lehmann
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
# GHDL for Windows - GHDL compile script
# Use 'compile.ps1 -Help' to see the integrated help page
# 
# .EXAMPLE
# C:\PS> .\compile.ps1 -Clean -Compile
# 
[CmdletBinding()]
param(
	# Display this help"
	[switch]$Help =			$false,
	
	# Slean up all files and directories
	[switch]$Clean =		$false,
	
	# Compile all targets
	[switch]$All =			$false,
	
	# Compile main targets
	[switch]$Compile =	$false,
		# Compile GHDL (simulator)
		[switch]$GHDL =		$false,
		# Undocumented
		[switch]$Test =		$false,
	
	# Build options
	# Build a release version
	[switch]$Release =	$false,
	# Set the back-end
	[string]$Backend =	"mcode",
	
	# Reduced messages
	[switch]$Quiet =						$false,
	# Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false,
	# Halt on errors
	[switch]$HaltOnError =			$false
)

# configure script here
$Script_RelPathToRoot =	"..\..\.."

# ---------------------------------------------
# save parameters and working directory
$Script_Parameters =	$args
$Script_ScriptDir =		$PSScriptRoot
$Script_WorkingDir =	Get-Location
$GHDLRootDir =				Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $Script_RelPathToRoot))

# set default values
$EnableVerbose =	$PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent
$EnableDebug =		$PSCmdlet.MyInvocation.BoundParameters["Debug"].IsPresent

Write-Host ("--> " + $Verbose + " value: " +$PSCmdlet.MyInvocation.BoundParameters["Verbose"] + " IsPresent: " + $PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent)
Write-Host ("--> " + $PSCommandPath + "  " + $PSBoundParameters + "  " + $PSCmdlet + "  " + $PSDefaultParameterValues)


# load modules from GHDL's 'libraries' directory
Import-Module $PSScriptRoot\shared.psm1 -Verbose:$false -ArgumentList "$Script_WorkingDir"
Import-Module $PSScriptRoot\targets.psm1 -Verbose:$false

# Display help if no command was selected
$Help = $Help -or (-not ($All -or $Compile -or $GHDL -or $Test -or $Clean))


Write-Host "================================================================================" -ForegroundColor Magenta
Write-Host "GHDL for Windows - GHDL and tools compile script" -ForegroundColor Magenta
Write-Host "================================================================================" -ForegroundColor Magenta

if ($Help)
{	Get-Help $MYINVOCATION.InvocationName -Detailed
	Exit-CompileScript
}

if ($All)
{	$Compile =	$true
}
if ($Compile)
{	$GHDL =			$true
	$Test =			$true
}

# configure some variables: paths, executables, directory names, ...
$BuildDirectoryName =											"build"

# Parameter checks
if ($Backend -ne "mcode")
{	Write-Host "[ERROR]: Back-end '$Backend' is not supported on Windows." -ForegroundColor Red
	Exit-CompileScript -1
}

# construct directories
$BinaryDestinationDirectory =		"$GHDLRootDir\$BuildDirectoryName\$Backend"
# construct executables
#$GHDLNewExecutable =								"$GHDLRootDir\$BuildDirectoryName\$Backend\bin\ghdl.exe"

# grep GHDL version string from Ada source file
$GHDLVersion = 							Get-GHDLVersion $GHDLRootDir
# compute some variables
$BuildRelease = if ($Release)	{	"Release"	}	else	{	"Development"	}
Write-Host "  Version:    $GHDLVersion"
Write-Host "  Release:    $BuildRelease"


$Git_IsGitRepo =						Test-GitRepository
# gather git information
if ($Git_IsGitRepo)
{	$Git_Branch_Name =				& git rev-parse --abbrev-ref HEAD
	$Git_Commit_DateString =	& git log -1 --format=%cd --date=short
	$Git_Commit_ShortHash =		& git rev-parse --short HEAD

	Write-Host "  Git branch: $Git_Branch_Name"
	Write-Host "  Git commit: $Git_Commit_DataString ($Git_Commit_ShortHash)"
}
Write-Host

if ($Release)
{	$BuildDirectory =		$BinaryDestinationDirectory
}
else
{	$BuildDirectory =		$BinaryDestinationDirectory
}





function Write-TargetResult($error)
{	if ($error)
	{	Write-Host "  [FAILED]"	-ForegroundColor Red		}
	# else
	# {	Write-Host "  [DONE]"		-ForegroundColor Green	}
}



# ==============================================================================
# Main Target: Clean
# ==============================================================================
if ($Clean)
{	$error = Invoke-Clean $BuildDirectory -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug
	Write-TargetResult $error
}	# Clean


# ==============================================================================
# Main Target: GHDL
# ==============================================================================
if ($GHDL)
{	# create a build directory
	$error = New-BuildDirectory $BuildDirectory
	Write-TargetResult $error
	

	# patch the version file if it's no release build
	if (-not $Release -and $Git_IsGitRepo)
	{	$error = Invoke-PatchVersionFile $GHDLRootDir $Git_Branch_Name $Git_Commit_DateString $Git_Commit_ShortHash
		Write-TargetResult $error
	}
	
	# build C source files
	$error = Invoke-CompileCFiles $GHDLRootDir $BinaryDestinationDirectory
	Write-TargetResult $error
	
	# build Ada source files
	$error = Invoke-CompileGHDLAdaFiles $GHDLRootDir $BinaryDestinationDirectory
	Write-TargetResult $error
	
	# strip result
	$error = Invoke-StripGHDLExecutable $BinaryDestinationDirectory
	Write-TargetResult $error
	
	# restore the version file if it was patched
	if (-not $Release -and $Git_IsGitRepo)
	{	$error = Restore-PatchedVersionFile $GHDLRootDir
		Write-TargetResult $error
	}
	
	
	
	
}

Write-Host "----  ENDE  ----"
Exit-CompileScript









if ($false)
{
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
