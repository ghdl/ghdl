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
#  PowerShell Script: Script to compile VHDL libraries for GHDL
#
# Description:
# ------------------------------------
#  This is a PowerShell script (executable) which:
#    - sets up a compilation environment
#    - test all dependencies
#    - pre processes VHDL files with GHDLFilter
#    - analyses VHDL files with GHDL
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
# 	GHDL for Windows - Library compile script
# 	Use 'compile-libraries.ps1 -Help' to see the integrated help page
# 
# .EXAMPLE
# 	C:\PS> .\compile-libraries.ps1 -Clean
# .EXAMPLE
# 	C:\PS> .\compile-libraries.ps1 -Compile -Verbose
# .EXAMPLE
# 	C:\PS> .\compile-libraries.ps1 -VHDL2008 -SuppressWarnings
# 
[CmdletBinding()]
param(
	# Display this help"
	[switch]$Help =							$false,
	
	# Clean up all files and directories
	[switch]$Clean =						$false,
	
	# Compile all library files
	[switch]$Compile =					$false,
	
	# Set VHDL Standard to '87
	[switch]$VHDL87 =						$false,
	# Set VHDL Standard to '93
	[switch]$VHDL93 =						$false,
	# Set VHDL Standard to '08
	[switch]$VHDL2008 =					$false,
	
	# Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false,
	# Halt on errors
	[switch]$HaltOnError =			$false,
	
	# Set GHDL executable
	[string]$GHDL =							"",
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
Import-Module $PSScriptRoot\shared.psm1 -Verbose:$false -Debug:$false -ArgumentList "$Script_WorkingDir", $Hosted

# Display help if no command was selected
$Help = $Help -or (-not ($Compile -or $VHDL87 -or $VHDL93 -or $VHDL2008 -or $Clean))

if ($Help)
{	Get-Help $MYINVOCATION.MyCommand.Path -Detailed
	Exit-CompileScript
}
if ($Compile)
{	$VHDL87 =		$true
	$VHDL93 =		$true
	$VHDL2008 =	$true
}

# configure some variables: paths, executables, directory names, ...
$VHDLLibrariesSourceDirectoryName =				"libraries"
$VHDLLibrariesDestinationDirectoryName =	"lib"
$BuildDirectoryName =											"build"
$Backend =																"mcode"

# construct directories
$VHDLSourceLibraryDirectory =				"$GHDLRootDir\$VHDLLibrariesSourceDirectoryName"
$VHDLDestinationLibraryDirectory =	"$GHDLRootDir\$BuildDirectoryName\$Backend\$VHDLLibrariesDestinationDirectoryName"
# construct executables
$GHDLNewExecutable =								"$GHDLRootDir\$BuildDirectoryName\$Backend\bin\ghdl.exe"


# Library sources
$SourceFiles = @{
	"std" = @(
		"textio",								"textio-body"
	);
	"ieee" = @(
		"std_logic_1164",				"std_logic_1164-body",
		"numeric_std",					"numeric_std-body",
		"numeric_bit",					"numeric_bit-body"
	);
	"math" = @(
		"math_real",						"math_real-body",
		"math_complex",					"math_complex-body"
	);
	"std08" = @(
		"textio",								"textio-body",
		"env",									"env-body"
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
		"vital_timing",					"vital_timing-body",
		"vital_primitives",			"vital_primitives-body"
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
		"std_logic_arith",			"std_logic_arith-body"
	)
}

if (-not $Hosted)
{	Write-Host "================================================================================" -ForegroundColor Yellow
	Write-Host "GHDL ($Backend) for Windows - Library compile script" -ForegroundColor Yellow
	Write-Host "================================================================================" -ForegroundColor Yellow
}

if ($Clean)
{	Write-Host "Removing all created files and directories..." -ForegroundColor Yellow
	if (Test-Path -Path $VHDLDestinationLibraryDirectory)
	{	$EnableVerbose	-and (Write-Host "  rmdir $VHDLDestinationLibraryDirectory")	| Out-Null
		Remove-Item $VHDLDestinationLibraryDirectory -Force -Recurse -ErrorAction SilentlyContinue
		if ($? -eq $false)
		{	Write-Host "[ERROR]: Cannot remove '$VHDLDestinationLibraryDirectory'." -ForegroundColor Red
			Exit-CompileScript -1
		}
	}
	if (-not ($VHDL87 -or $VHDL93 -or $VHDL2008))
	{	Exit-CompileScript	}
}

# get GHDL executable
if ($GHDL -ne "")
{	$GHDLExecutable = $GHDL								}
elseif (Test-Path env:GHDL)
{	$GHDLExecutable = $env:GHDL						}
elseif (Test-Path $GHDLNewExecutable -PathType Leaf)
{	$GHDLExecutable = $GHDLNewExecutable	}
else
{	$GHDLExecutable = "ghdl.exe"					}
	
if (-not (Test-Path $GHDLExecutable -PathType Leaf))
{	Write-Host "GHDL executable 'ghdl.exe' not found." -ForegroundColor Red
	Write-Host "Use adv. options '-GHDL' to set the GHDL executable." -ForegroundColor Yellow
	Exit-CompileScript -1
}


$ErrorCount = 0
if ($VHDL87 -or $VHDL93 -or $VHDL2008)
{	Write-Host "Compiling VHDL Libraries..."
	Write-Host "Preparing..."

	# create lib directory if it does not exist
	if (Test-Path -Path $VHDLDestinationLibraryDirectory)
	{	$EnableVerbose	-and (Write-Host "  Directory '$VHDLDestinationLibraryDirectory' already exists.")	| Out-Null
		
		# change working directory to VHDLDestinationLibraryDirectory
		$EnableVerbose	-and (Write-Host "  cd $VHDLDestinationLibraryDirectory")	| Out-Null
		Set-Location $VHDLDestinationLibraryDirectory
	
		$EnableVerbose	-and (Write-Host "  Cleaning up directory...")	| Out-Null
		Remove-Item ./* -Force -Recurse -ErrorAction SilentlyContinue
	}
	else
	{	$EnableVerbose	-and (Write-Host "  Creating directory '$VHDLDestinationLibraryDirectory'.")	| Out-Null
		New-Item -ItemType Directory -Path $VHDLDestinationLibraryDirectory -ErrorAction SilentlyContinue | Out-Null
		if (-not $?)
		{	Write-Host "[ERROR]: Cannot create destination directory '$VHDLDestinationLibraryDirectory'." -ForegroundColor Red
			Exit-CompileScript -1
		}
		
		# change working directory to VHDLDestinationLibraryDirectory
		$EnableVerbose	-and (Write-Host "  Change working directory to $VHDLDestinationLibraryDirectory")	| Out-Null
		Set-Location $VHDLDestinationLibraryDirectory
	}
	
	Write-Host
	Write-Host "Start compilation..."
}
# ============================================================================
# v87
# ============================================================================
if ($VHDL87)
{	$VHDLVersion =				"87"
	Write-Host "Compiling libraries for VHDL-$VHDLVersion" -ForegroundColor Cyan
	
	# ----------------------------------------------------------------------
	# v87\std
	# ----------------------------------------------------------------------
	$VHDLLibrary =				"std"
	Write-Host "  Compiling library '$VHDLLibrary'..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLLibrary\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "std"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C --bootstrap --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	# ----------------------------------------------------------------------
	# v87\ieee
	# ----------------------------------------------------------------------
	$VHDLLibrary =	"ieee"
	$VHDLFlavor =		"ieee"
	Write-Host "  Compiling library '$VHDLLibrary'..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLFlavor\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "ieee"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	# ----------------------------------------------------------------------
	# v87\synopsys
	# ----------------------------------------------------------------------
	$VHDLLibrary =	"ieee"
	$VHDLFlavor =		"synopsys"
	Write-Host "  Compiling library '$VHDLLibrary' ($VHDLFlavor)..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLFlavor\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "ieee"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	foreach ($SourceFile in $SourceFiles[$VHDLFlavor] + $SourceFiles["synopsys8793"])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLFlavor\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	$VHDLSourcesIndex = "vital95"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
}
# ============================================================================
# v93
# ============================================================================
if ($VHDL93)
{	$VHDLVersion =				"93"
	Write-Host "Compiling libraries for VHDL-$VHDLVersion" -ForegroundColor Cyan
	
	# ----------------------------------------------------------------------
	# v93\std
	# ----------------------------------------------------------------------
	$VHDLLibrary =				"std"
	Write-Host "  Compiling library '$VHDLLibrary'..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLLibrary\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "std"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C --bootstrap --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	# ----------------------------------------------------------------------
	# v93\ieee
	# ----------------------------------------------------------------------
	$VHDLLibrary =	"ieee"
	$VHDLFlavor =		"ieee"
	Write-Host "  Compiling library '$VHDLLibrary'..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLFlavor\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "ieee"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex] + $SourceFiles["math"])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	$VHDLSourcesIndex = "vital2000"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	# ----------------------------------------------------------------------
	# v93\synopsys
	# ----------------------------------------------------------------------
	$VHDLLibrary =	"ieee"
	$VHDLFlavor =		"synopsys"
	Write-Host "  Compiling library '$VHDLLibrary' ($VHDLFlavor)..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLFlavor\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "ieee"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex] + $SourceFiles["math"])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	foreach ($SourceFile in $SourceFiles[$VHDLFlavor] + $SourceFiles["synopsys8793"])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLFlavor\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	$VHDLSourcesIndex = "vital2000"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	# ----------------------------------------------------------------------
	# v93\mentor
	# ----------------------------------------------------------------------
	$VHDLLibrary =	"ieee"
	$VHDLFlavor =		"mentor"
	Write-Host "  Compiling library '$VHDLLibrary' ($VHDLFlavor)..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLFlavor\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "ieee"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex] + $SourceFiles["math"])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	foreach ($SourceFile in $SourceFiles[$VHDLFlavor])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLFlavor\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	$VHDLSourcesIndex = "vital2000"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
}
# ==============================================================================
# v08
# ==============================================================================
if ($VHDL2008)
{	$VHDLVersion =				"08"
	Write-Host "Compiling libraries for VHDL-$VHDLVersion" -ForegroundColor Cyan
	
	# ----------------------------------------------------------------------
	# v08\std
	# ----------------------------------------------------------------------
	$VHDLLibrary =				"std"
	Write-Host "  Compiling library '$VHDLLibrary'..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLLibrary\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "std08"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLLibrary\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C --bootstrap --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}

	# ----------------------------------------------------------------------
	# v08\ieee
	# ----------------------------------------------------------------------
	$VHDLLibrary =	"ieee"
	$VHDLFlavor =		"ieee"
	Write-Host "  Compiling library '$VHDLLibrary'..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLFlavor\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "ieee2008"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	$VHDLSourcesIndex = "vital2000"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" -frelaxed-rules --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}

	# ----------------------------------------------------------------------
	# v08\synopsys
	# ----------------------------------------------------------------------
	$VHDLLibrary =	"ieee"
	$VHDLFlavor =		"synopsys"
	Write-Host "  Compiling library '$VHDLLibrary' ($VHDLFlavor)..." -ForegroundColor DarkCyan
	
	$LibraryDirectory = "$VHDLDestinationLibraryDirectory\$VHDLFlavor\v$VHDLVersion"
	New-LibraryDirectory $LibraryDirectory	# $EnableVerbose
	Set-Location $LibraryDirectory
	
	$VHDLSourcesIndex = "ieee2008"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	foreach ($SourceFile in $SourceFiles[$VHDLFlavor])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLFlavor\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
	$VHDLSourcesIndex = "vital2000"
	foreach ($SourceFile in $SourceFiles[$VHDLSourcesIndex])
	{	Write-Host "    file: v$VHDLVersion\$SourceFile.v$VHDLVersion"
		$EnableVerbose -and	(Write-Host "      Patching file for VHDL-$VHDLVersion"																																														) | Out-Null
		$EnableDebug -and		(Write-Host "        Get-Content `"$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl`" -Encoding Ascii ``"	-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Format-VHDLSourceFile -Version `"$VHDLVersion`" ``"																						-ForegroundColor DarkGray	) | Out-Null
		$EnableDebug -and		(Write-Host "          | Out-File `"$SourceFile.v$VHDLVersion`" -Encoding Ascii"																				-ForegroundColor DarkGray	) | Out-Null
		# Patch file
		Get-Content "$VHDLSourceLibraryDirectory\$VHDLSourcesIndex\$SourceFile.vhdl" -Encoding Ascii `
			| Format-VHDLSourceFile -Version "$VHDLVersion" `
			| Out-File "$SourceFile.v$VHDLVersion" -Encoding Ascii
		
		# Analyze file
		$InvokeExpr = "$GHDLExecutable -a -C `"-P../std`" -frelaxed-rules --std=$VHDLVersion --work=$VHDLLibrary $SourceFile.v$VHDLVersion 2>&1"
		$EnableVerbose -and	(Write-Host "      Analyzing file '$SourceFile.v$VHDLVersion'"		) | Out-Null
		$EnableDebug -and		(Write-Host "        $InvokeExpr" -ForegroundColor DarkGray				) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "      "
		if (($LastExitCode -ne 0) -or -not $?)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	Exit-CompileScript -1		}
		}
	}
	
}	# $VHDL2008


Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling VHDL libraries " -NoNewline
if ($ErrorCount -gt 0)
{	Write-Host "[FAILED]" -ForegroundColor Red				}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green	}

Exit-CompileScript
