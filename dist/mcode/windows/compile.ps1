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
		C:\PS> .\compile.ps1 -Verbose -Compile
#>

# define script parameters
[CmdletBinding()]
Param(
	# compile GHDL
	[switch]$Compile,
	
	# clean up all files and directories
	[switch]$Clean,
	
	# display this help"
	[switch]$Help
)

# configure script here
$Script_RelPathToRoot =	"..\..\.."
$GHDLVersion = 					"0.33dev"

# save parameters and current working directory
$Script_Parameters =		$args
$Script_ScriptDir =			$PSScriptRoot
$Script_WorkingDir =		Get-Location
$GHDLRootDir_AbsPath =	Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $Script_RelPathToRoot))

# configure some variables: paths, executables, directory names, ...
$SourceDirName =				"src"
$BuildDirName =					"dist\mcode\build"

# TODO:
#	check if:
#		- program are installed / auto find programs / auto find paths
#		- program version
$GCCExecutable =				"gcc.exe"
$GNATExecutable =				"gnatmake.exe"
$StripExecutable =			"strip.exe"
$GHDLExecutable =				"ghdl.exe"
$GHDLFilterExecutable =	"ghdlfilter.exe"

# construct directories
$SourceDir =	$GHDLRootDir_AbsPath + "\" + $SourceDirName
$BuildDir =		$GHDLRootDir_AbsPath + "\" + $BuildDirName

# set default values
$Script_ExitCode = 			0
if ($PSCmdlet.MyInvocation.BoundParameters["Debug"].IsPresent) 		{	$Script_EnableDebug =		$true	}
if ($PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent)	{	$Script_EnableVerbose =	$true	}

# Compiler flags
$CFlags = @()				# start with an empty array
$CFlags += '-O'			# optimize; level ?
$CFlags += '-g'			# enable debug symbols

Write-Host "GHDL for Windows - GHDL and tools compile script" -ForegroundColor Yellow
Write-Host

if ($Help)
	{	Write-Host "Usage:"
		Write-Host "  compile.ps1 [-Verbose] [-Debug] (-Help|-Compile|-Clean)" -ForegroundColor Gray
		Write-Host
		Write-Host "Options:"
		Write-Host "  -Verbose    enable detailed messages"
		Write-Host "  -Debug      enable debug messages"
		Write-Host
		Write-Host "Commands:"
		Write-Host "  -Help       display this help"
		Write-Host "  -Compile    compile all library files"
		Write-Host "  -Clean      clean up all files and directories"
	}	# Help
elseif ($Clean)
 {	Write-Host "Removing all created files and directories..."
		Write-Host "  rmdir $BuildDir"
		
		Remove-Item $BuildDir -Force -Recurse -ErrorAction SilentlyContinue
	}	# Clean
elseif ($Compile)
 {	Write-Host "Compiling GHDL $GHDLVersion for Windows"
		Write-Host "Preparing..."

		# create build directory if it does not exist
		if (Test-Path -Path $BuildDir)
			{	Write-Host "  Directory '$BuildDir' already exists."}
		else
			{	Write-Host "  Creating directory '$BuildDir'."
				[void](New-Item -ItemType directory -Path $BuildDir -ErrorAction SilentlyContinue)
			}

		# change working directory to BuildDir
		Write-Host "  cd $BuildDir"
		Set-Location $BuildDir

		Write-Host
		Write-Host "Start compilation..."
		# list all files to be compiled; add additional CFlags if needed
		$SourceFiles = @()
		$SourceFiles += New-Object PSObject -Property @{File="grt\grt-cbinding.c";			CFlags=@()}
		$SourceFiles += New-Object PSObject -Property @{File="grt\grt-cvpi.c";					CFlags=@()}
		$SourceFiles += New-Object PSObject -Property @{File="grt\config\clock.c";			CFlags=@()}
		$SourceFiles += New-Object PSObject -Property @{File="grt\config\win32.c";			CFlags=@('-DWITH_GNAT_RUN_TIME')}
		$SourceFiles += New-Object PSObject -Property @{File="ortho\mcode\memsegs_c.c";	CFlags=@()}

		# compile c files
		foreach ($SourceFile in $SourceFiles)
			{	$Parameters = @()
				$Parameters += '-c'
				$Parameters += $CFlags
				$Parameters += $SourceFile.CFlags
				$Parameters += $SourceDir + "\" + $SourceFile.File
				
				Write-Host ("  compiling: " + $SourceFile.File)
				if ($Script_EnableDebug)
					{	Write-Host ("    file: " + $SourceDir + "\" + $SourceFile.File)
						Write-Host ("    call: " + $GCCExecutable + " " + ($Parameters -join ' '))
					}
				
				# call compiler
				& $GCCExecutable $Parameters
				if ($LastExitCode -ne 0)
					{	$Script_ExitCode = 1
						Write-Host "    ERROR while compiling" -ForegroundColor Red	
					}
			}

		if ($Script_ExitCode -eq 0)
			{	# compile with GNAT
				$Parameters = @()
				$Parameters += $CFlags
				$Parameters += '-gnatn'

				# add source include paths
				$Parameters += '-aI' + $GHDLRootDir_AbsPath + '\dist\mcode\windows'
				$Parameters += '-aI' + $SourceDir
				$Parameters += '-aI' + $SourceDir + '\ghdldrv'
				$Parameters += '-aI' + $SourceDir + '\psl'
				$Parameters += '-aI' + $SourceDir + '\grt'
				$Parameters += '-aI' + $SourceDir + '\ortho'
				$Parameters += '-aI' + $SourceDir + '\ortho\mcode'
				$Parameters += '-aI' + $SourceDir + '\vhdl'
				$Parameters += '-aI' + $SourceDir + '\vhdl\translate'
				$Parameters += 'ghdl_jit'

				# add output filename
				$Parameters += '-o'
				$Parameters += $GHDLExecutable

				# add linker parameters
				$Parameters += '-largs'
				$Parameters += 'grt-cbinding.o'
				$Parameters += 'clock.o'
				$Parameters += 'grt-cvpi.o'
				$Parameters += 'memsegs_c.o'
				$Parameters += 'win32.o'
				$Parameters += '-largs'
				$Parameters += '-Wl,--stack,8404992'

				# call compiler (GNAT)
				Write-Host "  compiling with GNAT"
				if ($Script_EnableDebug)
					{	#Write-Host ("    file: " + $SourceDir + "\" + $SourceFile.File)
						Write-Host ("    call: " + $GNATExecutable + " " + ($Parameters -join ' '))
					}

				& $GNATExecutable $Parameters
				if ($LastExitCode -ne 0)
					{	$Script_ExitCode = 1
						Write-Host "    ERROR while compiling" -ForegroundColor Red	}
			}
			
		if ($Script_ExitCode -eq 0)
			{	# 
				Write-Host "  stripping executable..."
				& $StripExecutable $GHDLExecutable
			}

		if ($Script_ExitCode -eq 0)
			{	# compile with GNAT
				$Parameters = @()
				$Parameters += $CFlags

				# add source include paths
				$Parameters += '-aI' + $GHDLRootDir_AbsPath + '\dist\mcode\windows'
				$Parameters += 'ghdlfilter'

				# add output filename
				$Parameters += '-o'
				$Parameters += $GHDLFilterExecutable

				# call compiler (GNAT)
				Write-Host "  compiling with GNAT"
				if ($Script_EnableDebug)
					{	#Write-Host ("    file: " + $SourceDir + "\" + $SourceFile.File)
						Write-Host ("    call: " + $GNATExecutable + " " + ($Parameters -join ' '))
					}

				& $GNATExecutable $Parameters
				if ($LastExitCode -ne 0)
					{	$Script_ExitCode = 1
						Write-Host "    ERROR while compiling" -ForegroundColor Red	}
			}
	}	# compile
else
	{	Write-Host "ERROR: missing argument(s)" -ForegroundColor Red
		Write-Host
		Write-Host "Usage:"
		Write-Host "  compile.ps1 [-Verbose] [-Debug] (-Help|-Compile|-Clean)" -ForegroundColor Gray
		Write-Host
	}	# unknown command
	
# restore working directory if changed
Set-Location $Script_WorkingDir

# return exit status
exit $Script_ExitCode
