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
#	Copyright (C) 2016-2018 Patrick Lehmann
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
$LibrarySourceFiles = @{
	"std" = @(
		"textio",								"textio_body"
	);
	"ieee" = @(
		"std_logic_1164",				"std_logic_1164_body",
		"numeric_std",					"numeric_std-body",
		"numeric_bit",					"numeric_bit-body"
	);
	"math" = @(
		"math_real",						"math_real-body",
		"math_complex",					"math_complex-body"
	);
	"std08" = @(
		"textio",								"textio_body",
		"env",									"env_body"
	);
	"ieee2008" = @(
		"std_logic_1164",				"std_logic_1164-body",
		"std_logic_textio",
		"math_real",						"math_real-body",
		"math_complex",					"math_complex-body",
		"numeric_bit",					"numeric_bit-body",
		"numeric_bit_unsigned",	"numeric_bit_unsigned-body",
		"numeric_std",					"numeric_std-body",
		"numeric_std_unsigned",	"numeric_std_unsigned-body",
		"fixed_float_types",
		"fixed_generic_pkg",		"fixed_generic_pkg-body",
		"fixed_pkg",
		"float_generic_pkg",		"float_generic_pkg-body",
		"float_pkg",
		"ieee_std_context",
		"ieee_bit_context"
	);
	"vital95" = @(
		"vital_timing",					"vital_timing_body",
		"vital_primitives",			"vital_primitives_body"
	);
	"vital2000" = @(
		"timing_p",							"timing_b",
		"prmtvs_p",							"prmtvs_b",
		"memory_p",							"memory_b"
	);
	"synopsys" = @(
		"std_logic_arith",
		"std_logic_unsigned",
		"std_logic_signed"
	);
	"synopsys8793" = @(
		"std_logic_textio",
		"std_logic_misc",				"std_logic_misc-body"
	);
	"mentor" = @(
		"std_logic_arith",			"std_logic_arith_body"
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

function Invoke-CompileLibraryVHDL87
{	<#
		.SYNOPSIS
		This CommandLet compiles the VHDL'87 library files.
		.PARAMETER BuildDirectory
		The directory where all analyzed VHDL files are stored.
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

	-not $Quiet			-and (Write-Host "Executing build target 'CompileLibraryVHDL87' ..." -ForegroundColor DarkCyan)  | Out-Null
	# $EnableVerbose	-and (Write-Host "  Removing all created files and directories..."                 )  | Out-Null
	# if (Test-Path -Path $BuildDirectory)
	# {	$EnableDebug		-and (Write-Host "    rmdir $BuildDirectory"                                     )  | Out-Null
		# Remove-Item $BuildDirectory -Force -Recurse -ErrorAction SilentlyContinue
		# if ($? -eq $false)
		# {	Write-Host "[ERROR]: Cannot remove '$BuildDirectory'." -ForegroundColor Red
			# throw "Cannot remove '$BuildDirectory'."
		# }
	# }
	# else
	# {	Write-Host "  [INFO] Directory '$BuildDirectory' does not exist." -ForegroundColor Yellow
	# }
}	# Invoke-CompileLibraryVHDL87

function Invoke-CompileLibraryVHDL93
{	<#
		.SYNOPSIS
		This CommandLet compiles the VHDL'93 library files.
		.PARAMETER BuildDirectory
		The directory where all analyzed VHDL files are stored.
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

	-not $Quiet			-and (Write-Host "Executing build target 'CompileLibraryVHDL93' ..." -ForegroundColor DarkCyan)  | Out-Null
	# $EnableVerbose	-and (Write-Host "  Removing all created files and directories..."                 )  | Out-Null
	# if (Test-Path -Path $BuildDirectory)
	# {	$EnableDebug		-and (Write-Host "    rmdir $BuildDirectory"                                     )  | Out-Null
		# Remove-Item $BuildDirectory -Force -Recurse -ErrorAction SilentlyContinue
		# if ($? -eq $false)
		# {	Write-Host "[ERROR]: Cannot remove '$BuildDirectory'." -ForegroundColor Red
			# throw "Cannot remove '$BuildDirectory'."
		# }
	# }
	# else
	# {	Write-Host "  [INFO] Directory '$BuildDirectory' does not exist." -ForegroundColor Yellow
	# }
}	# Invoke-CompileLibraryVHDL93

function Invoke-CompileLibraryVHDL08
{	<#
		.SYNOPSIS
		This CommandLet compiles the VHDL'08 library files.
		.PARAMETER BuildDirectory
		The directory where all analyzed VHDL files are stored.
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

	-not $Quiet			-and (Write-Host "Executing build target 'CompileLibraryVHDL08' ..." -ForegroundColor DarkCyan)  | Out-Null
	# $EnableVerbose	-and (Write-Host "  Removing all created files and directories..."                 )  | Out-Null
	# if (Test-Path -Path $BuildDirectory)
	# {	$EnableDebug		-and (Write-Host "    rmdir $BuildDirectory"                                     )  | Out-Null
		# Remove-Item $BuildDirectory -Force -Recurse -ErrorAction SilentlyContinue
		# if ($? -eq $false)
		# {	Write-Host "[ERROR]: Cannot remove '$BuildDirectory'." -ForegroundColor Red
			# throw "Cannot remove '$BuildDirectory'."
		# }
	# }
	# else
	# {	Write-Host "  [INFO] Directory '$BuildDirectory' does not exist." -ForegroundColor Yellow
	# }
}	# Invoke-CompileLibraryVHDL08

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

Export-ModuleMember -Function 'Invoke-CompileLibraryVHDL87'
Export-ModuleMember -Function 'Invoke-CompileLibraryVHDL93'
Export-ModuleMember -Function 'Invoke-CompileLibraryVHDL08'

Export-ModuleMember -Function 'Test-GHDLVersion'
