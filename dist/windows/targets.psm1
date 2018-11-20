# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
#
# ==============================================================================
#	Authors:						Patrick Lehmann
#
#	PowerShell Module:	The module provides build targets for GHDL.
#
# Description:
# ------------------------------------
#	This PowerShell module provides build targets for GHDL.
#
# ==============================================================================
#	Copyright (C) 2017-2018 Patrick Lehmann - Boetzingen, Germany
#	Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
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

# Library sources
$LibrarySourceFiles = [ordered]@{
	"std" = @(
		@{"Dir" = "std"; "File" = "textio";                         "IncludedIn" = @(1987, 1993, 2008); "Flavor" = @("all")},
		@{"Dir" = "std"; "File" = "textio-body";                    "IncludedIn" = @(1987, 1993, 2008); "Flavor" = @("all")},
		@{"Dir" = "std"; "File" = "env";                            "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "std"; "File" = "env-body";                       "IncludedIn" = @(            2008); "Flavor" = @("all")}
	);
	"ieee" = @(
		@{"Dir" = "ieee";     "File" = "std_logic_1164";            "IncludedIn" = @(1987, 1993      ); "Flavor" = @("all")},
		@{"Dir" = "ieee";     "File" = "std_logic_1164-body";       "IncludedIn" = @(1987, 1993      ); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "std_logic_1164";            "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "std_logic_1164-body";       "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "std_logic_textio";          "IncludedIn" = @(            2008); "Flavor" = @("ieee")}
		@{"Dir" = "ieee";     "File" = "numeric_std";               "IncludedIn" = @(1987, 1993      ); "Flavor" = @("all")},
		@{"Dir" = "ieee";     "File" = "numeric_std-body";          "IncludedIn" = @(1987, 1993      ); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "numeric_std";               "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "numeric_std-body";          "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "numeric_std_unsigned";      "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "numeric_std_unsigned-body"; "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee";     "File" = "numeric_bit";               "IncludedIn" = @(1987, 1993      ); "Flavor" = @("all")},
		@{"Dir" = "ieee";     "File" = "numeric_bit-body";          "IncludedIn" = @(1987, 1993      ); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "numeric_bit";               "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "numeric_bit-body";          "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "numeric_bit_unsigned";      "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "numeric_bit_unsigned-body"; "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "math_real";                 "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "math_real-body";            "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "math_complex";              "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "math_complex-body";         "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "fixed_float_types";         "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "fixed_generic_pkg";         "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "fixed_generic_pkg-body";    "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "fixed_pkg";                 "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "float_generic_pkg";         "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "float_generic_pkg-body";    "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "float_pkg";                 "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "ieee_std_context";          "IncludedIn" = @(            2008); "Flavor" = @("all")},
		@{"Dir" = "ieee2008"; "File" = "ieee_bit_context";          "IncludedIn" = @(            2008); "Flavor" = @("all")},

		@{"Dir" = "vital95";  "File" = "vital_timing";              "IncludedIn" = @(1987            ); "Flavor" = @("all")},
		@{"Dir" = "vital95";  "File" = "vital_timing-body";         "IncludedIn" = @(1987            ); "Flavor" = @("all")},
		@{"Dir" = "vital95";  "File" = "vital_primitives";          "IncludedIn" = @(1987            ); "Flavor" = @("all")},
		@{"Dir" = "vital95";  "File" = "vital_primitives-body";     "IncludedIn" = @(1987            ); "Flavor" = @("all")},
		
		@{"Dir" = "vital2000"; "File" = "timing_p";                 "IncludedIn" = @(      1993, 2008); "Flavor" = @("all")},
		@{"Dir" = "vital2000"; "File" = "timing_b";                 "IncludedIn" = @(      1993, 2008); "Flavor" = @("all")},
		@{"Dir" = "vital2000"; "File" = "prmtvs_p";                 "IncludedIn" = @(      1993, 2008); "Flavor" = @("all")},
		@{"Dir" = "vital2000"; "File" = "prmtvs_b";                 "IncludedIn" = @(      1993, 2008); "Flavor" = @("all")},
		@{"Dir" = "vital2000"; "File" = "memory_p";                 "IncludedIn" = @(      1993, 2008); "Flavor" = @("all")},
		@{"Dir" = "vital2000"; "File" = "memory_b";                 "IncludedIn" = @(      1993, 2008); "Flavor" = @("all")},
		
		@{"Dir" = "synopsys"; "File" = "std_logic_arith";           "IncludedIn" = @(1987, 1993, 2008); "Flavor" = @("synopsys")},
		@{"Dir" = "synopsys"; "File" = "std_logic_unsigned";        "IncludedIn" = @(1987, 1993, 2008); "Flavor" = @("synopsys")},
		@{"Dir" = "synopsys"; "File" = "std_logic_signed";          "IncludedIn" = @(1987, 1993, 2008); "Flavor" = @("synopsys")},
		@{"Dir" = "synopsys"; "File" = "std_logic_textio";          "IncludedIn" = @(1987, 1993      ); "Flavor" = @("synopsys")},
		@{"Dir" = "synopsys"; "File" = "std_logic_misc";            "IncludedIn" = @(1987, 1993      ); "Flavor" = @("synopsys")},
		@{"Dir" = "synopsys"; "File" = "std_logic_misc-body";       "IncludedIn" = @(1987, 1993      ); "Flavor" = @("synopsys")},

		@{"Dir" = "mentor";   "File" = "std_logic_arith";           "IncludedIn" = @(      1993, 2008); "Flavor" = @("mentor")},
		@{"Dir" = "mentor";   "File" = "std_logic_arith-body";      "IncludedIn" = @(      1993, 2008); "Flavor" = @("mentor")}
	)
}

function Invoke-CleanGHDL
{	<#
		.SYNOPSIS
		This CommandLet removes all generated GHDL files.
		.PARAMETER BuildDirectory
		The directory where all generated GHDL files are stored.
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[string]	$BuildDirectory,
		[switch]	$Quiet = $false
	)

	$EnableDebug =		-not $Quiet -and (                  $PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug  -or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])

	-not $Quiet			-and (Write-Host "Executing build target 'CleanGHDL' ..." -ForegroundColor DarkCyan)  | Out-Null
	$EnableVerbose	-and (Write-Host "  Removing all created files and directories..."                 )  | Out-Null
	if (Test-Path -Path $BuildDirectory)
	{	$EnableDebug		-and (Write-Host "    rmdir $BuildDirectory"                                     )  | Out-Null
		Remove-Item $BuildDirectory -Force -Recurse -ErrorAction SilentlyContinue
		if ($? -eq $false)
		{	Write-Host "[ERROR]: Cannot remove '$BuildDirectory'." -ForegroundColor Red
			throw "Cannot remove '$BuildDirectory'."
		}
	}
	else
	{	Write-Host "  [INFO] Directory '$BuildDirectory' does not exist." -ForegroundColor Yellow
	}
}	# Invoke-CleanGHDL

function Invoke-CleanLibraries
{	<#
		.SYNOPSIS
		This CommandLet removes all generated library files.
		.PARAMETER LibraryDirectory
		The directory where all generated library files are stored.
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[string]	$LibraryDirectory,
		[switch]	$Quiet = $false
	)

	$EnableDebug =		-not $Quiet -and (                  $PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug  -or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])

	-not $Quiet			-and (Write-Host "Executing build target 'CleanLibraries' ..." -ForegroundColor DarkCyan) | Out-Null
	$EnableVerbose	-and (Write-Host "  Removing all created library files and directories..."              ) | Out-Null
	if (Test-Path -Path $LibraryDirectory)
	{	$EnableDebug		-and (Write-Host "    rmdir $LibraryDirectory"                                        ) | Out-Null
		Remove-Item $LibraryDirectory -Force -Recurse -ErrorAction SilentlyContinue
		if ($? -eq $false)
		{	Write-Host "[ERROR]: Cannot remove '$LibraryDirectory'." -ForegroundColor Red
			throw "Cannot remove '$LibraryDirectory'."
		}
	}
	else
	{	Write-Host "  [INFO] Directory '$LibraryDirectory' does not exist." -ForegroundColor Yellow
	}
}	# Invoke-CleanLibraries

function Invoke-CleanPackageZip
{	<#
		.SYNOPSIS
		This CommandLet removes all generated package files.
		.PARAMETER PackageDirectory
		The directory where all files are stored for packaging.
		.PARAMETER PackageFile
		The package file.
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[string]	$PackageDirectory,
		[string]	$PackageFile,
		[switch]	$Quiet = $false
	)

	$EnableDebug =		-not $Quiet -and (                  $PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug  -or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])

	-not $Quiet			-and (Write-Host "Executing build target 'CleanPackageZip' ..." -ForegroundColor DarkCyan)  | Out-Null
	$EnableVerbose	-and (Write-Host "  Removing all created files and directories..."                       )  | Out-Null
	if (Test-Path -Path $PackageDirectory)
	{	$EnableDebug		-and (Write-Host "    rmdir $PackageDirectory"                                         )  | Out-Null
		Remove-Item $PackageDirectory -Force -Recurse -ErrorAction SilentlyContinue
		if ($? -eq $false)
		{	Write-Host "[ERROR]: Cannot remove '$PackageDirectory'." -ForegroundColor Red
			throw "Cannot remove '$PackageDirectory'."
		}
	}
	else
	{	Write-Host "  [INFO] Directory '$PackageDirectory' does not exist." -ForegroundColor Yellow
	}
	
	$EnableVerbose	-and (Write-Host "  Removing Zip file..."   ) | Out-Null
	if (Test-Path -Path $PackageFile)
	{	$EnableDebug		-and (Write-Host "    rmdir $PackageFile" ) | Out-Null
		Remove-Item $PackageFile -Force -Recurse -ErrorAction SilentlyContinue
		if ($? -eq $false)
		{	Write-Host "[ERROR]: Cannot remove '$PackageFile'." -ForegroundColor Red
			throw "Cannot remove '$PackageFile'."
		}
	}
	else
	{	Write-Host "  [INFO] Directory '$PackageFile' does not exist." -ForegroundColor Yellow
	}
}	# Invoke-CleanPackageZip

function Invoke-CleanPackagePS1
{	<#
		.SYNOPSIS
		This CommandLet removes all generated package files.
		.PARAMETER PackageDirectory
		The directory where all files are stored for packaging.
		.PARAMETER PackageFile
		The package file.
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		# [string]	$PackageDirectory,
		[string]	$PackageFile,
		[switch]	$Quiet = $false
	)

	$EnableDebug =		-not $Quiet -and (                  $PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug  -or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])

	-not $Quiet			-and (Write-Host "Executing build target 'CleanPackagePS1' ..." -ForegroundColor DarkCyan) | Out-Null
	# $EnableVerbose	-and (Write-Host "  Removing all created files and directories..."                        ) | Out-Null
	# if (Test-Path -Path $PackageDirectory)
	# {	$EnableDebug		-and (Write-Host "    rmdir $PackageDirectory"                                          ) | Out-Null
		# Remove-Item $PackageDirectory -Force -Recurse -ErrorAction SilentlyContinue
		# if ($? -eq $false)
		# {	Write-Host "[ERROR]: Cannot remove '$PackageDirectory'." -ForegroundColor Red
			# throw "Cannot remove '$PackageDirectory'."
		# }
	# }
	# else
	# {	Write-Host "  [INFO] Directory '$PackageDirectory' does not exist." -ForegroundColor Yellow
	# }
	
	$EnableVerbose	-and (Write-Host "  Removing PS1 file..."   ) | Out-Null
	if (Test-Path -Path $PackageFile)
	{	$EnableDebug		-and (Write-Host "    rmdir $PackageFile" ) | Out-Null
		Remove-Item $PackageFile -Force -Recurse -ErrorAction SilentlyContinue
		if ($? -eq $false)
		{	Write-Host "[ERROR]: Cannot remove '$PackageFile'." -ForegroundColor Red
			throw "Cannot remove '$PackageFile'."
		}
	}
	else
	{	Write-Host "  [INFO] Directory '$PackageFile' does not exist." -ForegroundColor Yellow
	}
}	# Invoke-CleanPackagePS1


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

	Write-Host "Executing build target 'BuildDirectory' ..." -ForegroundColor DarkCyan
	if (Test-Path -Path $BuildDirectory -PathType Container)
	{	-not $Quiet -and (Write-Host "  [INFO] Directory '$BuildDirectory' already exists." -ForegroundColor Yellow) 	| Out-Null	}
	else
	{	-not $Quiet  -and (Write-Host "  Creating new directory '$BuildDirectory'."                                )  | Out-Null
		$EnableDebug -and (Write-Host "    mkdir $BuildDirectory"                                                  )  | Out-Null
		New-Item -ItemType Directory -Path $BuildDirectory -ErrorAction SilentlyContinue                              | Out-Null
		if ($? -eq $false)
		{	Write-Host "[ERROR]: Cannot create '$BuildDirectory'." -ForegroundColor Red
			throw "Cannot create '$BuildDirectory'."
		}
	}
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
		Throw "File '$ConfigureFilePath' not found."
	}
	$FileContent = Get-Content -Path $ConfigureFilePath
	foreach ($Line in $FileContent)
	{	if ($Line -match 'ghdl_version=\"(.+?)\"')
		{ return $Matches[1]	}
	}
	Write-Host "[ERROR]: RegExp didn't match in '$ConfigureFilePath'." -ForegroundColor Red
	Throw "'ghdl_version' not found in file Throw '$ConfigureFilePath'."
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

	Write-Host "Executing build target 'PatchVersionFile' ..." -ForegroundColor DarkCyan

	if (-not (Test-Path -Path $VersionInputFilePath -PathType Leaf))
	{	Write-Host "[ERROR]: Version file '$VersionInputFilePath' does not exists." -ForegroundColor Red
		throw "Version file '$VersionInputFilePath' does not exists."
	}
	-not $Quiet -and (Write-Host "  Patching '$VersionInputFilePath'.") | Out-Null
	$FileContent = Get-Content -Path $VersionInputFilePath -Encoding Ascii
	if ($? -eq $false)
	{	Write-Host "[ERROR]: While opening '$VersionInputFilePath'." -ForegroundColor Red
		throw "While opening '$VersionInputFilePath'."
	}
	
  $GHDLVersion = Get-GHDLVersion $GHDLRootDir
	$FileContent = $FileContent -Replace "@VER@", $GHDLVersion
	$FileContent = $FileContent -Replace "\(tarball\)", " (commit: $GitCommitDataString;  git branch: $GitBranchName';  hash: $GitCommitHash) "


	$FileContent | Out-File $VersionFilePath -Encoding Ascii
	if ($? -eq $false)
	{	Write-Host "[ERROR]: While writing to '$VersionFilePath'." -ForegroundColor Red
		throw "While writing to '$VersionFilePath'."
	}
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
	Write-Host "Executing build target 'CompileCFiles' ..." -ForegroundColor DarkCyan

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
		$InvokeExpr = "$Prog_GCC " + ($Parameters -join " ") + " 2>&1"

		Write-Host ("  compiling: " + $SourceFile.File)
		Write-Debug	"    call: $InvokeExpr"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGCCLine -Indent "    "
		if ($LastExitCode -ne 0)
		{	Write-Host ("[ERROR]: While compiling '{0}'." -f $SourceFile.File) -ForegroundColor Red
			throw "While compiling '{0}'."
		}
	}
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
	Write-Host "Executing build target 'CompileGHDLAdaFiles' ..." -ForegroundColor DarkCyan

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
	$Parameters += 'grt-cbinding.o'
	$Parameters += 'clock.o'
	$Parameters += 'grt-cvpi.o'
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
		throw "While compiling '$GHDL_Mcode_Name'."
	}
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
	Write-Host "Executing build target 'StripGHDLExecutable' ..." -ForegroundColor DarkCyan

	# call striping tool (strip)
	Write-Host "  stripping '$GHDL_Mcode_Name'"
	Write-Debug "    call: $Prog_Strip $GHDL_Mcode_Name"
	& $Prog_Strip $GHDL_Mcode_Name
	if ($LastExitCode -ne 0)
	{	Write-Host "[ERROR]: While stripping '$GHDL_Mcode_Name'." -ForegroundColor Red
		throw "While stripping '$GHDL_Mcode_Name'."
	}
}	# Invoke-StripGHDLExecutable

function Invoke-PrepareCompileLibrary
{	<#
		.SYNOPSIS
		This CommandLet prepares the target directories for the library compilation.
		.PARAMETER DestinationDirectory
		The directory where all analyzed artifacts will be staored stored.
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[string]	$DestinationDirectory,
		[switch]	$Quiet = $false
	)

	$EnableDebug =		-not $Quiet -and (                  $PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug  -or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])

	-not $Quiet			-and (Write-Host "Executing build target 'PrepareCompileLibrary' ..." -ForegroundColor DarkCyan)  | Out-Null

	# create lib directory if it does not exist
	if (Test-Path -Path $DestinationDirectory)
	{	$EnableVerbose	-and (Write-Host "${Indentation}  Directory '$DestinationDirectory' already exists." -ForegroundColor Yellow )	| Out-Null
		
		# change working directory to DestinationDirectory
		$EnableVerbose  -and (Write-Host "${Indentation}  Changing working directory..." -ForegroundColor Gray    ) | Out-Null
		$EnableDebug    -and (Write-Host "${Indentation}    cd $DestinationDirectory"    -ForegroundColor DarkGray) | Out-Null
		Set-Location $DestinationDirectory
	
		$EnableVerbose  -and (Write-Host "${Indentation}  Cleaning up directory..."                                         -ForegroundColor Gray     ) | Out-Null
		$EnableDebug    -and (Write-Host "${Indentation}    Remove-Item ./* -Force -Recurse -ErrorAction SilentlyContinue"  -ForegroundColor DarkGray ) | Out-Null
		Remove-Item ./* -Force -Recurse -ErrorAction SilentlyContinue
	}
	else
	{	$EnableVerbose	-and (Write-Host "${Indentation}  Creating directory '$DestinationDirectory'." -ForegroundColor Gray    ) | Out-Null
		$EnableDebug    -and (Write-Host "${Indentation}    mkdir $DestinationDirectory"               -ForegroundColor DarkGray) | Out-Null
		try
		{	mkdir $DestinationDirectory | Out-Null  }
		catch
		{	throw "Cannot create destination directory '$DestinationDirectory'." }
		
		# change working directory to DestinationDirectory
		$EnableVerbose  -and (Write-Host "${Indentation}  Changing working directory..." -ForegroundColor Gray    ) | Out-Null
		$EnableDebug    -and (Write-Host "${Indentation}    cd $DestinationDirectory"    -ForegroundColor DarkGray) | Out-Null
		Set-Location $DestinationDirectory
	}
}

function Invoke-CompileLibrary
{	<#
		.SYNOPSIS
		This CommandLet compiles the VHDL'87 library files.
		.PARAMETER VHDLLibrarySourceDirectory
		Undocumented
		.PARAMETER VHDLLibraryDestinationDirectory
		Undocumented
		.PARAMETER VHDLVersionYear
		VHDL version
		.PARAMETER SuppressWarnings
		Undocumented
		.PARAMETER HaltOnError
		Undocumented
		.PARAMETER Indentation
		Undocumented
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[Parameter(Mandatory=$true)][string]$VHDLLibrarySourceDirectory,
		[Parameter(Mandatory=$true)][string]$VHDLLibraryDestinationDirectory,
		[Parameter(Mandatory=$true)][ValidateSet("1987", "1993", "2008")][string]$VHDLVersionYear,
		[Parameter(Mandatory=$false)][bool]$SuppressWarnings = $false,
		[Parameter(Mandatory=$false)][bool]$HaltOnError = $true,
		[Parameter(Mandatory=$false)][string]$Indentation = "",
		[Parameter(Mandatory=$false)][switch]$Quiet = $false
	)

	$EnableDebug =		-not $Quiet -and (                  $PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug  -or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])

	-not $Quiet -and (Write-Host "Executing build target 'CompileLibrary' ($VHDLVersionYear, ieee) ..." -ForegroundColor DarkCyan) | Out-Null

	$GHDLOptions = @("-C", " -frelaxed-rules")
	$VHDLVersion = switch ($VHDLVersionYear)
	{	"1987" { "87" }
		"1993" { "93" }
		"2008" { "08" }
	}
	
	foreach ($Library in $LibrarySourceFiles.GetEnumerator())
	{	$LibraryName = $Library.Name
		$SourceFiles = @()
		$Options = $GHDLOptions

		if ($LibraryName -eq "std")
		{$Options += "--bootstrap"  }
		
		Write-Host "${Indentation}Patching files for '$LibraryName' ..." -ForegroundColor Yellow
		$EnableVerbose -and (Write-Host "${Indentation}  Creating library $LibraryName in '$LibraryName' ..."	-ForegroundColor Gray	) | Out-Null
		if (Test-Path -Path "$VHDLLibraryDestinationDirectory\$LibraryName\v$VHDLVersion")
		{ $EnableVerbose -and (Write-Host "${Indent}  [INFO] Library directory '$LibraryName' already exists." -ForegroundColor Yellow ) | Out-Null }
		else
		{	$EnableDebug -and   (Write-Host "${Indentation}    mkdir $VHDLLibraryDestinationDirectory\$LibraryName\v$VHDLVersion"	-ForegroundColor DarkGray	) | Out-Null
			mkdir "$VHDLLibraryDestinationDirectory\$LibraryName\v$VHDLVersion" | Out-Null
		}
		
		foreach ($FileEntry in $Library.Value)
		{	if (($VHDLVersionYear -in $FileEntry["IncludedIn"]) -and (("all" -in $FileEntry["Flavor"]) -or ("ieee" -in $FileEntry["Flavor"])))
			{	$SourceFile =      $FileEntry["File"]
				$SourceDirectory = $FileEntry["Dir"]
				Write-Host "$Indentation  Patching file '$SourceDirectory\$SourceFile.vhdl' for VHDL-$VHDLVersionYear to '$LibraryName\v$VHDLVersion\$SourceFile.v$VHDLVersion'" -ForegroundColor Gray
				$EnableDebug -and   (Write-Host "$Indentation    Get-Content `"$VHDLLibrarySourceDirectory\$SourceDirectory\$SourceFile.vhdl`" -Encoding Ascii ``"                       -ForegroundColor DarkGray  ) | Out-Null
				$EnableDebug -and   (Write-Host "$Indentation      | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"                                                                 -ForegroundColor DarkGray  ) | Out-Null
				$EnableDebug -and   (Write-Host "$Indentation      | Out-File `"$VHDLLibraryDestinationDirectory\$LibraryName\v$VHDLVersion\$SourceFile.v$VHDLVersion`" -Encoding Ascii" -ForegroundColor DarkGray  ) | Out-Null
				# Patch file
				Get-Content "$VHDLLibrarySourceDirectory\$SourceDirectory\$SourceFile.vhdl" -Encoding Ascii `
					| Format-VHDLSourceFile -Version "$VHDLVersion" `
					| Out-File "$VHDLLibraryDestinationDirectory\$LibraryName\v$VHDLVersion\$SourceFile.v$VHDLVersion" -Encoding Ascii
			
				$SourceFiles += "$VHDLLibraryDestinationDirectory\$LibraryName\v$VHDLVersion\$SourceFile.v$VHDLVersion"
			}
		}
		
		try
		{	Analyze-Library $VHDLLibraryDestinationDirectory $LibraryName $SourceFiles "$VHDLVersion" $Options -SuppressWarnings:$SuppressWarnings -HaltOnError:$HaltOnError -Indentation:$Indentation -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
		catch
		{	throw $_  }
	}
}	# Invoke-CompileLibrary

function Invoke-CompileIEEELibraryFlavor
{	<#
		.SYNOPSIS
		This CommandLet compiles the VHDL'87 library files.
		.PARAMETER VHDLLibrarySourceDirectory
		Undocumented
		.PARAMETER VHDLLibraryDestinationDirectory
		Undocumented
		.PARAMETER VHDLVersionYear
		VHDL version
		.PARAMETER VHDLFlavor
		VHDL flavor
		.PARAMETER SuppressWarnings
		Undocumented
		.PARAMETER HaltOnError
		Undocumented
		.PARAMETER Indentation
		Undocumented
		.PARAMETER Quiet
		Disable outputs to the host console.
	#>
	[CmdletBinding()]
	param(
		[Parameter(Mandatory=$true)][string]$VHDLLibrarySourceDirectory,
		[Parameter(Mandatory=$true)][string]$VHDLLibraryDestinationDirectory,
		[Parameter(Mandatory=$true)][ValidateSet("1987", "1993", "2008")][string]$VHDLVersionYear,
		[Parameter(Mandatory=$true)][ValidateSet("ieee", "synopsys", "mentor")][string]$VHDLFlavor,
		[Parameter(Mandatory=$false)][bool]$SuppressWarnings = $false,
		[Parameter(Mandatory=$false)][bool]$HaltOnError = $true,
		[Parameter(Mandatory=$false)][string]$Indentation = "",
		[Parameter(Mandatory=$false)][switch]$Quiet = $false
	)

	$EnableDebug =		-not $Quiet -and (                  $PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug  -or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])

	-not $Quiet -and (Write-Host "Executing build target 'CompileLibrary' ($VHDLVersionYear, $VHDLFlavor) ..." -ForegroundColor DarkCyan) | Out-Null

	$GHDLOptions = @("-C")
	$VHDLVersion = switch ($VHDLVersionYear)
	{	"1987" { "87" }
		"1993" { "93" }
		"2008" { "08" }
	}
	
	$SourceFiles =  @()
	$Options =      $GHDLOptions
	$OutputDir =    $VHDLFlavor
	
	Write-Host "${Indentation}Patching files for 'ieee' ..." -ForegroundColor Yellow
	$EnableVerbose -and (Write-Host "${Indentation}  Creating library ieee in '$VHDLFlavor' ..."	-ForegroundColor Gray	) | Out-Null
	if (Test-Path -Path "$VHDLLibraryDestinationDirectory\$OutputDir\v$VHDLVersion")
	{ $EnableVerbose -and (Write-Host "${Indent}  [INFO] Library directory '$VHDLFlavor' already exists." -ForegroundColor Yellow ) | Out-Null }
	else
	{	$EnableDebug -and   (Write-Host "${Indentation}    mkdir $VHDLLibraryDestinationDirectory\$OutputDir\v$VHDLVersion"	-ForegroundColor DarkGray	) | Out-Null
		mkdir "$VHDLLibraryDestinationDirectory\$OutputDir\v$VHDLVersion" | Out-Null
	}
	
	foreach ($FileEntry in $LibrarySourceFiles["ieee"])
	{	if (($VHDLVersionYear -in $FileEntry["IncludedIn"]) -and (("all" -in $FileEntry["Flavor"]) -or ($VHDLFlavor -in $FileEntry["Flavor"])))
		{	$SourceFile =      $FileEntry["File"]
			$SourceDirectory = $FileEntry["Dir"]
			Write-Host "$Indentation  Patching file '$SourceDirectory\$SourceFile.vhdl' for VHDL-$VHDLVersion to '$OutputDir\v$VHDLVersion\$SourceFile.v$VHDLVersion'"             -ForegroundColor Gray
			$EnableDebug -and   (Write-Host "$Indentation    Get-Content `"$VHDLLibrarySourceDirectory\$SourceDirectory\$SourceFile.vhdl`" -Encoding Ascii ``"                     -ForegroundColor DarkGray  ) | Out-Null
			$EnableDebug -and   (Write-Host "$Indentation      | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"                                                               -ForegroundColor DarkGray  ) | Out-Null
			$EnableDebug -and   (Write-Host "$Indentation      | Out-File `"$VHDLLibraryDestinationDirectory\$OutputDir\v$VHDLVersion\$SourceFile.v$VHDLVersion`" -Encoding Ascii" -ForegroundColor DarkGray  ) | Out-Null
			# Patch file
			Get-Content "$VHDLLibrarySourceDirectory\$SourceDirectory\$SourceFile.vhdl" -Encoding Ascii `
				| Format-VHDLSourceFile -Version "$VHDLVersion" `
				| Out-File "$VHDLLibraryDestinationDirectory\$OutputDir\v$VHDLVersion\$SourceFile.v$VHDLVersion" -Encoding Ascii
		
			$SourceFiles += "$VHDLLibraryDestinationDirectory\$OutputDir\v$VHDLVersion\$SourceFile.v$VHDLVersion"
		}
	}
	
	try
	{	Analyze-Library $VHDLLibraryDestinationDirectory "ieee" $SourceFiles "$VHDLVersion" $Options -LibraryDirectory:$VHDLFlavor -SuppressWarnings:$SuppressWarnings -HaltOnError:$HaltOnError -Indentation:$Indentation -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	throw $_  }
}	# Invoke-CompileIEEELibraryFlavor


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
	Write-Host "Executing build target 'GHDLVersion' ..." -ForegroundColor DarkCyan

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
		throw "While executing '$GHDL_Mcode_Name'."
	}
}	# Test-GHDLVersion


# export functions
Export-ModuleMember -Function 'Get-GHDLVersion'

Export-ModuleMember -Function 'Invoke-CleanGHDL'
Export-ModuleMember -Function 'Invoke-CleanLibraries'
Export-ModuleMember -Function 'Invoke-CleanPackageZip'
Export-ModuleMember -Function 'Invoke-CleanPackagePS1'

Export-ModuleMember -Function 'New-BuildDirectory'

Export-ModuleMember -Function 'Invoke-PatchVersionFile'
Export-ModuleMember -Function 'Restore-PatchedVersionFile'

Export-ModuleMember -Function 'Invoke-CompileCFiles'
Export-ModuleMember -Function 'Invoke-CompileGHDLAdaFiles'
Export-ModuleMember -Function 'Invoke-StripGHDLExecutable'

Export-ModuleMember -Function 'Invoke-PrepareCompileLibrary'
Export-ModuleMember -Function 'Invoke-CompileLibrary'
Export-ModuleMember -Function 'Invoke-CompileIEEELibraryFlavor'

Export-ModuleMember -Function 'Test-GHDLVersion'
