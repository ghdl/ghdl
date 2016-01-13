# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	PowerShell Module:	The module provides build targets for GHDL.
# 
#	Authors:						Patrick Lehmann
# 
# Description:
# ------------------------------------
#	This PowerShell module provides build targets for GHDL.
#
# ==============================================================================
#	Copyright (C) 2016 Patrick Lehmann
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

# TODO:
#	check if:
#		- program are installed / auto find programs / auto find paths
#		- program version

# configure compiler tools
$GCCExecutable =					"gcc.exe"
$GNATMakeExecutable =			"gnatmake.exe"
$StripExecutable =				"strip.exe"

# configure output file
$GHDLExecutableName =			"ghdl.exe"
$FilterExecutable =				"filter.exe"

# configure directory structure
$CommonSourceDirName =		"src"
$WinMcodeSourceDirName =	"dist\mcode\windows"
# $WinLLVMSourceDirName =		"dist\llvm\windows"
$WinMcodeBuildDirName =		"dist\mcode\build"
# $WinLLVMBuildDirName =		"dist\llvm\build"

# construct file paths
$VersionFileName =				"version.ads"


function Invoke-Clean
{	<#
		.SYNOPSIS
		This CommandLet removes all generated files.
		.PARAMETER BuildDirectory
		The directory where all generated files are stored.
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[string]	$BuildDirectory,
		[switch]	$Quiet = $false
	)
	
	Write-Host "Executing build target 'Clean' ..." -ForegroundColor Yellow
	if ($Quiet -eq $false)
	{	Write-Host "  Removing all created files and directories..."
		Write-Host "    rmdir $BuildDirectory"
	}
	Remove-Item $BuildDirectory -Force -Recurse -ErrorAction SilentlyContinue
	
	return $false
}	# Invoke-Clean

function Invoke-CreateBuildDirectory
{	<#
		.SYNOPSIS
		This CommandLet creates a build directory if not existent, yet.
		.PARAMETER BuildDirectory
		The directory where all generated files are stored.
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[string]	$BuildDirectory,
		[switch]	$Quiet = $false
	)
	
	Write-Host "Executing build target 'CreateBuildDirectory' ..." -ForegroundColor Yellow
	if (Test-Path -Path $BuildDirectory)
	{	if ($Quiet -eq $false)
		{	Write-Host "  Directory '$BuildDirectory' already exists."		}
	}
	else
	{	if ($Quiet -eq $false)
		{	Write-Host "  Creating new directory '$BuildDirectory'."		}
		
		[void](New-Item -ItemType directory -Path $BuildDirectory -ErrorAction SilentlyContinue)
	}
	
	return $false
}	# Invoke-CreateBuildDirectory

function Get-GHDLVersion
{	<#
		.SYNOPSIS
		Returns the GHDL version string.
		.PARAMETER GHDLRootDir
		The repository root directory.
	#>
	[CmdletBinding()]
	param(
		[string]	$GHDLRootDir
	)
	# construct DirectoryPaths
	$SourceDirectory =		$GHDLRootDir + "\" + $CommonSourceDirName
	# construct FilePaths
	$VersionFilePath =		$SourceDirectory + "\" + $VersionFileName
	
	if (-not (Test-Path -Path $VersionFilePath))
	{	Write-Host "  Version file '$VersionFilePath' does not exists." -ForegroundColor Red
		return ""
	}
	$FileContent = Get-Content -Path $VersionFilePath
	foreach ($Line in $FileContent)
	{	if ($Line -match 'Ghdl_Ver(.+?)\"(.+?)\";')
		{ return $Matches[2]	}
	}
	return ""
}

function Invoke-PatchVersionFile
{	<#
		.SYNOPSIS
		This CommandLet patches the version file to include the git patch state.
		.PARAMETER GHDLRootDir
		The repository root directory.
		.PARAMETER GitBranchName
		The branch's name, where HEAD is located.
		.PARAMETER GitCommitDataString
		The DateTime when HEAD was commited.
		.PARAMETER GitCommitHash
		The Hash of HEAD.
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[string]	$GHDLRootDir,
		[string]	$GitBranchName = "unknown",
		[string]	$GitCommitDataString = "unknown",
		[string]	$GitCommitHash = "........",
		[switch]	$Quiet = $false
	)
	# construct DirectoryPaths
	$SourceDirectory =					$GHDLRootDir + "\" + $CommonSourceDirName
	# construct FilePaths
	$CurrentVersionFilePath =		$SourceDirectory + "\" + $VersionFileName
	$OriginalVersionFilePath =	$SourceDirectory + "\" + $VersionFileName + ".bak"
	
	Write-Host "Executing build target 'PatchVersionFile' ..." -ForegroundColor Yellow
	
	if (-not (Test-Path -Path $CurrentVersionFilePath))
	{	Write-Host "  Version file '$CurrentVersionFilePath' does not exists." -ForegroundColor Red
		return $true
	}
	if ($Quiet -eq $false)
	{	Write-Host "  Patching '$CurrentVersionFilePath'."		}
	$FileContent = Get-Content -Path $CurrentVersionFilePath -Encoding Ascii
	$FileContent = $FileContent -Replace "\s\(\d+\)\s", " (commit: $GitCommitDataString;  git branch: $GitBranchName';  hash: $GitCommitHash) "
	
	Move-Item $CurrentVersionFilePath $OriginalVersionFilePath -Force
	$FileContent | Out-File $CurrentVersionFilePath -Encoding Ascii
	
	return $false
}	# Invoke-PatchVersionFile

function Restore-PatchedVersionFile
{	<#
		.SYNOPSIS
		This CommandLet restores the original version file.
		.PARAMETER GHDLRootDir
		The repository root directory.
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[string]	$GHDLRootDir,
		[switch]	$Quiet = $false
	)
	# construct DirectoryPaths
	$SourceDirectory =					$GHDLRootDir + "\" + $CommonSourceDirName
	# construct FilePaths
	$CurrentVersionFilePath =		$SourceDirectory + "\" + $VersionFileName
	$OriginalVersionFilePath =	$SourceDirectory + "\" + $VersionFileName + ".bak"
	
	Write-Host "Executing build target 'PatchedVersionFile' ..." -ForegroundColor Yellow
	
	if (-not (Test-Path -Path "$CurrentVersionFilePath"))
	{	Write-Host "  Version file '$CurrentVersionFilePath' does not exists." -ForegroundColor Red
		return $true
	}
	if ($Quiet -eq $false)
	{	Write-Host "  Restoring '$CurrentVersionFilePath'."		}
	Move-Item $OriginalVersionFilePath $CurrentVersionFilePath -Force
	return $false
}	# Restore-PatchedVersionFile

function Get-CFlags
{	<#
		.SYNOPSIS
		Returns common ANSI C compiler flags for GCC.
	#>
	return @(
		"-O1",		# optimize; level 1
		"-g"			# enable debug symbols
	)
}

function Invoke-CompileCFiles
{	<#
		.SYNOPSIS
		This CommandLet compiles all C files with GCC.
		.PARAMETER GHDLRootDir
		The repository root directory.
		.PARAMETER BuildDirectory
		The directory where all generated files are stored.
		.PARAMETER Quiet
		Disable outputs to the host console
	#>
	[CmdletBinding()]
	param(
		[string]	$GHDLRootDir,
		[string]	$BuildDirectory,
		[switch]	$Quiet = $false
	)
	# construct DirectoryPaths
	$SourceDirectory =					$GHDLRootDir + "\" + $CommonSourceDirName
	
	Set-Location $BuildDirectory
	Write-Host "Executing build target 'CompileCFiles' ..." -ForegroundColor Yellow

	# list all files to be compiled; add additional CFlags if needed
	$SourceFiles = @()
	$SourceFiles += New-Object PSObject -Property @{File="grt\grt-cbinding.c";			CFlags=@()}
	$SourceFiles += New-Object PSObject -Property @{File="grt\grt-cvpi.c";					CFlags=@()}
	$SourceFiles += New-Object PSObject -Property @{File="grt\config\clock.c";			CFlags=@()}
	$SourceFiles += New-Object PSObject -Property @{File="grt\config\win32.c";			CFlags=@('-DWITH_GNAT_RUN_TIME')}
	$SourceFiles += New-Object PSObject -Property @{File="ortho\mcode\memsegs_c.c";	CFlags=@()}

	# compile C files
	foreach ($SourceFile in $SourceFiles)
	{	$Parameters = @()
		$Parameters += "-c"											# compile only
		$Parameters += Get-CFlags								# append common CFlags
		$Parameters += $SourceFile.CFlags
		$Parameters += $SourceDirectory + "\" + $SourceFile.File
		
		# call C compiler
		$InvokeExpr = "$GCCExecutable " + ($Parameters -join " ") + " 2>&1"
		
		Write-Host ("  compiling: " + $SourceFile.File)
		Write-Debug	"    call: $InvokeExpr"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine
		if ($LastExitCode -ne 0)
		{	return $true		}
	}
	
	return $false
}	# Invoke-CompileCFiles


function Invoke-CompileGHDLAdaFiles
{	<#
		.SYNOPSIS
		This CommandLet compiles all Ada files with GNAT.
		.PARAMETER GHDLRootDir
		The repository root directory.
		.PARAMETER BuildDirectory
		The directory where all generated files are stored.
		.PARAMETER Quiet
		Disable outputs to the host console
	#>
	[CmdletBinding()]
	param(
		[string]	$GHDLRootDir,
		[string]	$BuildDirectory,
		[switch]	$Quiet = $false
	)
	# construct DirectoryPaths
	$CommonSourceDirectory =				$GHDLRootDir + "\" + $CommonSourceDirName
	$WinMcodeSourceDirectory =			$GHDLRootDir + "\" + $WinMcodeSourceDirName
	
	Set-Location $BuildDirectory
	Write-Host "Executing build target 'CompileGHDLAdaFiles' ..." -ForegroundColor Yellow
	
	$Parameters = @()
	$Parameters += Get-CFlags								# append common CFlags
	$Parameters += '-gnatn'

	# append all source paths
	$Parameters += '-aI' + $WinMcodeSourceDirectory
	$Parameters += '-aI' + $CommonSourceDirectory
	$Parameters += '-aI' + $CommonSourceDirectory + '\ghdldrv'
	$Parameters += '-aI' + $CommonSourceDirectory + '\psl'
	$Parameters += '-aI' + $CommonSourceDirectory + '\grt'
	$Parameters += '-aI' + $CommonSourceDirectory + '\ortho'
	$Parameters += '-aI' + $CommonSourceDirectory + '\ortho\mcode'
	$Parameters += '-aI' + $CommonSourceDirectory + '\vhdl'
	$Parameters += '-aI' + $CommonSourceDirectory + '\vhdl\translate'
	
	# top level
	$Parameters += 'ghdl_jit'

	# add output filename
	$Parameters += '-o'
	$Parameters += $GHDLExecutableName

	# append linker parameters
	$Parameters += '-largs'
	$Parameters += 'grt-cbinding.o'
	$Parameters += 'clock.o'
	$Parameters += 'grt-cvpi.o'
	$Parameters += 'memsegs_c.o'
	$Parameters += 'win32.o'
	$Parameters += '-ldbghelp'
	$Parameters += '-largs'
	# $Parameters += '-Wl,--stack,8404992'

	# call Ada compiler (GNAT)
	$InvokeExpr = "$GNATMakeExecutable " + ($Parameters -join " ") + " 2>&1"
	
	Write-Host "  compiling with GNAT"
	Write-Debug "    call: $InvokeExpr"
	$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine
	return ($LastExitCode -ne 0)
}	# Invoke-CompileGHDLAdaFiles

function Invoke-CompileFilterAdaFiles
{	<#
		.SYNOPSIS
		This CommandLet compiles all Ada files with GNAT.
		.PARAMETER SourceDirectory
		The directory where all source files are located.
		.PARAMETER BuildDirectory
		The directory where all generated files are stored.
		.PARAMETER Quiet
		Disable outputs to the host console
	#>
	[CmdletBinding()]
	param(
		[string]	$GHDLRootDir,
		[string]	$BuildDirectory,
		[switch]	$Quiet = $false
	)
	# construct DirectoryPaths
	$SourceDirectory =					$GHDLRootDir + "\" + $CommonSourceDirName
	
	Set-Location $BuildDirectory
	Write-Host "Executing build target 'CompileFilterAdaFiles' ..." -ForegroundColor Yellow
	
	$Parameters = @()
	$Parameters += Get-CFlags								# append common CFlags
	$Parameters += '-gnatn'

	# append all source paths
	$Parameters += '-aI' + $SourceDirectory + '\..\dist\mcode\windows'
	
	# top level
	$Parameters += 'ghdlfilter'

	# add output filename
	$Parameters += '-o'
	$Parameters += $FilterExecutable

	# call Ada compiler (GNAT)
	$InvokeExpr = "$GNATMakeExecutable " + ($Parameters -join " ") + " 2>&1"
	
	Write-Host "  compiling with GNAT"
	Write-Debug "    call: $InvokeExpr"
	$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine
	return ($LastExitCode -ne 0)
}	# Invoke-CompileFilterAdaFiles

function Invoke-StripGHDLExecutable
{	<#
		.SYNOPSIS
		This CommandLet strips the result files.
		.PARAMETER BuildDirectory
		The directory where all generated files are stored.
		.PARAMETER Quiet
		Disable outputs to the host console
	#>
	[CmdletBinding()]
	param(
		[string]	$BuildDirectory,
		[switch]	$Quiet = $false
	)

	Set-Location $BuildDirectory
	Write-Host "Executing build target 'StripGHDLExecutable' ..." -ForegroundColor Yellow
	
	# call striping tool (strip)
	Write-Host "  stripping '$GHDLExecutableName'"
	Write-Debug "    call: $StripExecutable $GHDLExecutableName"
	& $StripExecutable $GHDLExecutableName
	return ($LastExitCode -ne 0)
}	# Invoke-StripGHDLExecutable

function Test-GHDLVersion
{	<#
		.SYNOPSIS
		This CommandLet executes ghdl to read the version information
		.PARAMETER BuildDirectory
		The directory where all generated files are stored.
		.PARAMETER Quiet
		Disable outputs to the host console
	#>
	[CmdletBinding()]
	param(
		[string]	$BuildDirectory,
		[switch]	$Quiet = $false
	)

	Set-Location $BuildDirectory
	Write-Host "Executing build target 'GHDLVersion' ..." -ForegroundColor Yellow
	
	if (-not (Test-Path -Path $GHDLExecutableName))
	{	Write-Host "  GHDL executable '$GHDLExecutableName' does not exists." -ForegroundColor Red
		return $true
	}
	
	# call ghdl
	$InvokeExpr = "$GHDLExecutableName --version 2>&1"
	
	Write-Host "  executing '$GHDLExecutableName'"
	Write-Host "    call: $InvokeExpr"
	Write-Host "----------------------------------------"
	Invoke-Expression $InvokeExpr | Restore-NativeCommandStream
	Write-Host "----------------------------------------"
	return ($LastExitCode -ne 0)
}	# Test-GHDLVersion


# export functions
Export-ModuleMember -Function 'Get-GHDLVersion'
Export-ModuleMember -Function 'Invoke-Clean'
Export-ModuleMember -Function 'Invoke-CreateBuildDirectory'
Export-ModuleMember -Function 'Invoke-PatchVersionFile'
Export-ModuleMember -Function 'Restore-PatchedVersionFile'
Export-ModuleMember -Function 'Invoke-CompileCFiles'
Export-ModuleMember -Function 'Invoke-CompileGHDLAdaFiles'
Export-ModuleMember -Function 'Invoke-CompileFilterAdaFiles'
Export-ModuleMember -Function 'Invoke-StripGHDLExecutable'
Export-ModuleMember -Function 'Test-GHDLVersion'
