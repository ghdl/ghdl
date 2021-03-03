# EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
#
# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
#  PowerShell Module:	The module provides build targets for GHDL.
#
# Description:
# ------------------------------------
#  This PowerShell module provides build targets for GHDL.
#
# ==============================================================================
#  Copyright (C) 2016-2017 Patrick Lehmann
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

# TODO:
#	check if:
#		- program are installed / auto find programs / auto find paths
#		- program version

# configure compiler tools
$Prog_GCC =								"gcc.exe"
$Prog_GNATMake =					"gnatmake.exe"
$Prog_Strip =							"strip.exe"

# configure output file
$GHDL_Mcode_Name =				"ghdl.exe"

# configure directory structure
$CommonSourceDirName =		"src"
$WinMcodeSourceDirName =	"dist\windows\mcode"
# $WinLLVMSourceDirName =		"dist\windows\llvm"

# construct file paths
$VersionFileName_In =				"version.in"
$VersionFileName_Ads =			"version.ads"


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

	$EnableDebug =		-not $Quiet -and (									$PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug	-or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])

	-not $Quiet			-and (Write-Host "Executing build target 'Clean' ..." -ForegroundColor Yellow)	| Out-Null
	$EnableVerbose	-and (Write-Host "  Removing all created files and directories..."						)	| Out-Null
	if (Test-Path -Path $BuildDirectory)
	{	$EnableDebug		-and (Write-Host "    rmdir $BuildDirectory"																	)	| Out-Null
		Remove-Item $BuildDirectory -Force -Recurse -ErrorAction SilentlyContinue
		if ($? -eq $false)
		{	Write-Host "[ERROR]: Cannot remove '$BuildDirectory'." -ForegroundColor Red
			return $true
		}
	}
	return $false
}	# Invoke-Clean

function New-BuildDirectory
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

	Write-Host "Executing build target 'BuildDirectory' ..." -ForegroundColor Yellow
	if (Test-Path -Path $BuildDirectory -PathType Container)
	{	-not $Quiet -and (Write-Host "  Directory '$BuildDirectory' already exists."	) 	| Out-Null	}
	else
	{	-not $Quiet -and (Write-Host "  Creating new directory '$BuildDirectory'."		) 	| Out-Null
		New-Item -ItemType Directory -Path $BuildDirectory -ErrorAction SilentlyContinue	| Out-Null
		if ($? -eq $false)
		{	Write-Host "[ERROR]: Cannot create '$BuildDirectory'." -ForegroundColor Red
			return $true
		}
	}

	return $false
}	# New-BuildDirectory

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
	$ConfigureFilePath =		$GHDLRootDir + "\configure"

	if (-not (Test-Path -Path $ConfigureFilePath -PathType Leaf))
	{	Write-Host "[ERROR]: Version file '$ConfigureFilePath' does not exists." -ForegroundColor Red
		return $true
	}
	$FileContent = Get-Content -Path $ConfigureFilePath
	foreach ($Line in $FileContent)
	{	if ($Line -match 'ghdl_version=\"(.+?)\"')
		{ return $Matches[2]	}
	}
	Write-Host "[ERROR]: RegExp didn't match in '$ConfigureFilePath'." -ForegroundColor Red
	return $true
}	# Get-GHDLVersion

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
		[string]	$GitBranchName =				"unknown",
		[string]	$GitCommitDataString =	"unknown",
		[string]	$GitCommitHash =				"........",
		[switch]	$Quiet =								$false
	)
	# construct DirectoryPaths
	$SourceDirectory =				$GHDLRootDir + "\" + $CommonSourceDirName
	$VersionInputFilePath =		$SourceDirectory + "\" + $VersionFileName_In
	$VersionFilePath =				$SourceDirectory + "\" + $VersionFileName_Ads

	Write-Host "Executing build target 'PatchVersionFile' ..." -ForegroundColor Yellow

	if (-not (Test-Path -Path $VersionInputFilePath -PathType Leaf))
	{	Write-Host "[ERROR]: Version file '$VersionInputFilePath' does not exists." -ForegroundColor Red
		return $true
	}
	-not $Quiet -and (Write-Host "  Patching '$VersionInputFilePath'.") | Out-Null
	$FileContent = Get-Content -Path $VersionInputFilePath -Encoding Ascii
	if ($? -eq $false)
	{	Write-Host "[ERROR]: While opening '$VersionInputFilePath'." -ForegroundColor Red
		return $true
	}
  $GHDLVersion =            Get-GHDLVersion $GHDLRootDir
	$FileContent = $FileContent -Replace "\s@VER@\s", $GHDLVersion

	$FileContent = $FileContent -Replace "\s\(tarball\)\s", " (commit: $GitCommitDataString;  git branch: $GitBranchName';  hash: $GitCommitHash) "


	$FileContent | Out-File $VersionFilePath -Encoding Ascii
	if ($? -eq $false)
	{	Write-Host "[ERROR]: While writing to '$VersionFilePath'." -ForegroundColor Red
		return $true
	}

	return $false
}	# Invoke-PatchVersionFile


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
	$SourceFiles += New-Object PSObject -Property @{File="grt\grt-cstdio.c";		  	CFlags=@()}
	$SourceFiles += New-Object PSObject -Property @{File="grt\grt-cvpi.c";					CFlags=@()}
	$SourceFiles += New-Object PSObject -Property @{File="grt\grt-cvhpi.c";					CFlags=@()}
	$SourceFiles += New-Object PSObject -Property @{File="grt\grt-cdynload.c";		  CFlags=@()}
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
		$InvokeExpr = "$Prog_GCC " + ($Parameters -join " ") + " 2>&1"

		Write-Host ("  compiling: " + $SourceFile.File)
		Write-Debug	"    call: $InvokeExpr"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGCCLine -Indent "    "
		if ($LastExitCode -ne 0)
		{	Write-Host ("[ERROR]: While compiling '{0}'." -f $SourceFile.File) -ForegroundColor Red
			return $true
		}
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
	$Parameters += $GHDL_Mcode_Name

	# append linker parameters
	$Parameters += '-largs'
	$Parameters += 'grt-cstdio.o'
	$Parameters += 'clock.o'
	$Parameters += 'grt-cvpi.o'
	$Parameters += 'grt-cvhpi.o'
	$Parameters += 'grt-cdynload.o'
	$Parameters += 'memsegs_c.o'
	$Parameters += 'win32.o'
	$Parameters += '-ldbghelp'
	$Parameters += '-largs'
	# $Parameters += '-Wl,--stack,8404992'

	# call Ada compiler (GNAT)
	$InvokeExpr = "$Prog_GNATMake " + ($Parameters -join " ") + " 2>&1"

	Write-Host "  compiling with GNAT"
	Write-Debug "    call: $InvokeExpr"
	$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGCCLine -Indent "    "
	if ($LastExitCode -ne 0)
	{	Write-Host "[ERROR]: While compiling '$GHDL_Mcode_Name'." -ForegroundColor Red
		return $true
	}
	return $false
}	# Invoke-CompileGHDLAdaFiles


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
	Write-Host "  stripping '$GHDL_Mcode_Name'"
	Write-Debug "    call: $Prog_Strip $GHDL_Mcode_Name"
	& $Prog_Strip $GHDL_Mcode_Name
	if ($LastExitCode -ne 0)
	{	Write-Host "[ERROR]: While stripping '$GHDL_Mcode_Name'." -ForegroundColor Red
		return $true
	}
	return $false
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

	if (-not (Test-Path -Path $GHDL_Mcode_Name -PathType Leaf))
	{	Write-Host "  GHDL executable '$GHDL_Mcode_Name' does not exists." -ForegroundColor Red
		return $true
	}

	# call ghdl
	$InvokeExpr = "$GHDL_Mcode_Name --version 2>&1"

	Write-Host "  executing '$GHDL_Mcode_Name'"
	Write-Host "    call: $InvokeExpr"
	Write-Host "    ----------------------------------------"
	Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-HostExtended "    "
	Write-Host "    ----------------------------------------"
	if ($LastExitCode -ne 0)
	{	Write-Host "[ERROR]: While executing '$GHDL_Mcode_Name'." -ForegroundColor Red
		return $true
	}
	return $false
}	# Test-GHDLVersion


# export functions
Export-ModuleMember -Function 'Get-GHDLVersion'
Export-ModuleMember -Function 'Invoke-Clean'
Export-ModuleMember -Function 'New-BuildDirectory'
Export-ModuleMember -Function 'Invoke-PatchVersionFile'
Export-ModuleMember -Function 'Restore-PatchedVersionFile'
Export-ModuleMember -Function 'Invoke-CompileCFiles'
Export-ModuleMember -Function 'Invoke-CompileGHDLAdaFiles'
Export-ModuleMember -Function 'Invoke-StripGHDLExecutable'
Export-ModuleMember -Function 'Test-GHDLVersion'
