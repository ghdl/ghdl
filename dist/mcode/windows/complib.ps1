# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	PowerShell Script:	Script to compile VHDL libraries for GHDL
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
#		- pre processes VHDL files with GHDLFilter
#		- analyses VHDL files with GHDL
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
		GHDL for Windows - Library compile script
		Use 'complib.ps1 -Help' to see the integrated help page
	
	.EXAMPLE
		C:\PS> .\complib.ps1 -Verbose -Compile
	.EXAMPLE
		C:\PS> .\complib.ps1 -Verbose -Clean
#>

# define script parameters
[CmdletBinding()]
Param(
	# compile all library files
	[switch]$Compile,
	
	# clean up all files and directories
	[switch]$Clean,
	
	# display this help"
	[switch]$Help
)

# configure script here
$Script_RelPathToRoot =	"..\..\.."

# set default values
$Script_ExitCode = 			0
if ($PSCmdlet.MyInvocation.BoundParameters["Debug"].IsPresent) 		{	$Script_EnableDebug =		$true	}
if ($PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent)	{	$Script_EnableVerbose =	$true	}

# save parameters and current working directory
$Script_Parameters =		$args
$Script_ScriptDir =			$PSScriptRoot
$Script_WorkingDir =		Get-Location
$GHDLRootDir_AbsPath =	Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $Script_RelPathToRoot))

# configure some variables: paths, executables, directory names, ...
#$VHDLSourceLibraryDirName =	"libraries"
#$VHDLDestLibraryDirName =		"lib"

$GHDLExecutable =				$GHDLRootDir_AbsPath + "\dist\mcode\build\ghdl.exe"
$GHDLFilterExecutable =	$GHDLRootDir_AbsPath + "\dist\mcode\build\ghdlfilter.exe"

# construct directories
$VHDLSourceLibraryDir =	$GHDLRootDir_AbsPath + "\libraries"	# + $VHDLSourceLibraryDirName
$VHDLDestLibraryDir =		$GHDLRootDir_AbsPath + "\dist\mcode\lib"	# + $VHDLDestLibraryDirName


Write-Host "GHDL for Windows - Library compile script" -ForegroundColor Yellow
Write-Host

if ($Help)
	{	Write-Host "Usage:"
		Write-Host "  complib.ps1 [-Verbose] [-Debug] (-Help|-Compile|-Clean)" -ForegroundColor Gray
		Write-Host
		Write-Host "Options:"
		Write-Host "  -Verbose    enable detailed messages"
		Write-Host "  -Debug      enable debug messages"
		Write-Host
		Write-Host "Commands:"
		Write-Host "  -Help       display this help"
		Write-Host "  -Compile    compile all library files"
		Write-Host "  -Clean      clean up all files and directories"
	}
elseif ($Clean)
	{	Write-Host "Removing all created files and directories..."
		Write-Host "  rmdir $VHDLDestLibraryDir"
		
		Remove-Item $VHDLDestLibraryDir -Force -Recurse -ErrorAction SilentlyContinue
	}
elseif ($Compile)
	{	Write-Host "Compiling VHDL Libraries..."
		Write-Host "Preparing..."

		# create lib directory if it does not exist
		if (Test-Path -Path $VHDLDestLibraryDir)
			{	Write-Host "  Directory '$VHDLDestLibraryDir' already exists."
				
				# change working directory to VHDLDestLibraryDir
				Write-Host "  cd $VHDLDestLibraryDir"
				Set-Location $VHDLDestLibraryDir
			
				Write-Host "  cleaning up directory..."
				Remove-Item ./* -Force -Recurse -ErrorAction SilentlyContinue
			}
		else
			{	Write-Host "  Creating directory '$VHDLDestLibraryDir'."
				[void](New-Item -ItemType directory -Path $VHDLDestLibraryDir -ErrorAction SilentlyContinue)
				
				# change working directory to VHDLDestLibraryDir
				Write-Host "  cd $VHDLDestLibraryDir"
				Set-Location $VHDLDestLibraryDir
			}


		# Library sources
		$SourceFiles = @{
			"std" = @(
				"textio",
				"textio_body"
			);
			"ieee" = @(
				"std_logic_1164",
				"std_logic_1164_body",
				"numeric_std",
				"numeric_std-body",
				"numeric_bit",
				"numeric_bit-body"
			);
			"math" = @(
				"math_real",
				"math_real-body",
				"math_complex",
				"math_complex-body"
			);
			"std08" = @(
				"textio",
				"textio_body",
				"env",
				"env_body"
			);
			"ieee2008" = @(
				"std_logic_1164",
				"std_logic_1164-body",
				"std_logic_textio",
				"math_real",
				"math_real-body",
				"math_complex",
				"math_complex-body",
				"numeric_bit",
				"numeric_bit-body",
				"numeric_bit_unsigned",
				"numeric_bit_unsigned-body",
				"numeric_std",
				"numeric_std-body",
				"numeric_std_unsigned",
				"numeric_std_unsigned-body",
				"fixed_float_types",
				"fixed_generic_pkg",
				"fixed_generic_pkg-body",
				"fixed_pkg",
				"float_generic_pkg",
				"float_generic_pkg-body",
				"float_pkg",
				"ieee_std_context",
				"ieee_bit_context"
			);
			"vital95" = @(
				"vital_timing",
				"vital_timing_body",
				"vital_primitives",
				"vital_primitives_body"
			);
			"vital2000" = @(
				"timing_p",
				"timing_b",
				"prmtvs_p",
				"prmtvs_b",
				"memory_p",
				"memory_b"
			);
			"synopsys" = @(
				"std_logic_arith",
				"std_logic_textio",
				"std_logic_unsigned",
				"std_logic_signed",
				"std_logic_misc",
				"std_logic_misc-body"
			);
			"mentor" = @(
				"std_logic_arith",
				"std_logic_arith_body"
			)
		}
		
		Write-Host
		Write-Host "Start compilation..."

# ==============================================================================
# v87
# ==============================================================================
		# create 'v87' directory if it does not exist
		$LocalDirName = "v87"
		$LocalDir = $VHDLDestLibraryDir + "\" + $LocalDirName
		if (Test-Path -Path $LocalDir)
			{	if ($Script_EnableVerbose) {	Write-Host "  Directory '$LocalDirName' already exists."	}		}
		else
			{	Write-Host "  Creating directory '$LocalDirName'."
				[void](New-Item -ItemType directory -Path $LocalDir -ErrorAction SilentlyContinue)
			}
		
		Write-Host "  compiling into $LocalDirName"
		
		# ----------------------------------------------------------------------
		# v87\std
		# ----------------------------------------------------------------------
		if ($Script_ExitCode -eq 0)
			{	$VHDLDestLibrary = "std"
				
				# create 'std' directory if it does not exist
				$LocalDir2Name = $VHDLDestLibrary
				$LocalDir2 = $LocalDir + "\" + $LocalDir2Name
				
				Write-Host "    compiling library $VHDLDestLibrary" -ForegroundColor DarkCyan
				if (Test-Path -Path $LocalDir2)
					{	if ($Script_EnableVerbose) {	Write-Host "      Directory '$LocalDir2Name' already exists."		}	}
				else
					{	Write-Host "      Creating directory '$LocalDir2Name'."
						[void](New-Item -ItemType Directory -Path $LocalDir2 -ErrorAction SilentlyContinue)
					}

				# change working directory to LocalDir2
				Write-Host "      cd $LocalDir2"
				Set-Location $LocalDir2

				$VHDLSrcLibrary = "std"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v87\$SourceFile.v87"
						if ($Script_EnableVerbose) {	Write-Host "        ghdlfilter (-v87)"	}
						Get-Content "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" -Encoding Ascii -Raw `
							| & $GHDLFilterExecutable @('-v87') `
							| Out-File "$SourceFile.v87" -Encoding Ascii
						
						#Write-Host "Press any key to continue..."
						#[void]($Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown"))
						#$Host.UI.RawUI.FlushInputBuffer()
						
						$GHDLParameters = @("-a", "-C", "--std=87", "--bootstrap", "--work=$VHDLDestLibrary", "$SourceFile.v87")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
			}
		
		#$Script_ExitCode = 1
		
		# ----------------------------------------------------------------------
		# v87\ieee
		# ----------------------------------------------------------------------
		if ($Script_ExitCode -eq 0)
			{	$VHDLDestLibrary = "ieee"
				
				# create 'ieee' directory if it does not exist
				$LocalDir2Name = $VHDLDestLibrary
				$LocalDir2 = $LocalDir + "\" + $LocalDir2Name
				
				Write-Host "    compiling library $VHDLDestLibrary" -ForegroundColor DarkCyan
				if (Test-Path -Path $LocalDir2)
					{		if ($Script_EnableVerbose) {	Write-Host "      Directory '$LocalDir2Name' already exists."		}	}
				else
					{	Write-Host "      Creating directory '$LocalDir2Name'."
						[void](New-Item -ItemType directory -Path $LocalDir2 -ErrorAction SilentlyContinue)
					}

				# change working directory to LocalDir2
				Write-Host "      cd $LocalDir2"
				Set-Location $LocalDir2

				$VHDLSrcLibrary = "ieee"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v87\$SourceFile.v87"
						if ($Script_EnableVerbose) {	Write-Host "        ghdlfilter (-v87)"	}
						Get-Content "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" -Encoding Ascii -Raw `
							| & $GHDLFilterExecutable @('-v87') `
							| Out-File "$SourceFile.v87" -Encoding Ascii
						
						$GHDLParameters = @("-a", "-C", "--std=87", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.v87")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
				
				$VHDLSrcLibrary = "vital95"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v87\$SourceFile.v87"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=87", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
			}
		
		# ----------------------------------------------------------------------
		# v87\synopsys
		# ----------------------------------------------------------------------
		if ($Script_ExitCode -eq 0)
			{	$VHDLDestLibrary = "ieee"
				
				# create 'synopsys' directory if it does not exist
				$LocalDir2Name = "synopsys"
				$LocalDir2 = $LocalDir + "\" + $LocalDir2Name
				
				Write-Host "    compiling library $VHDLDestLibrary" -ForegroundColor DarkCyan
				if (Test-Path -Path $LocalDir2)
					{		if ($Script_EnableVerbose) {	Write-Host "      Directory '$LocalDir2Name' already exists."		}	}
				else
					{	Write-Host "      Creating directory '$LocalDir2Name'."
						[void](New-Item -ItemType directory -Path $LocalDir2 -ErrorAction SilentlyContinue)
					}

				# change working directory to LocalDir2
				Write-Host "      cd $LocalDir2"
				Set-Location $LocalDir2

				$VHDLSrcLibrary = "ieee"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v87\$SourceFile.v87"
						if ($Script_EnableVerbose) {	Write-Host "        ghdlfilter (-v87)"	}
						Get-Content "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" -Encoding Ascii -Raw `
							| & $GHDLFilterExecutable @('-v87') `
							| Out-File "$SourceFile.v87" -Encoding Ascii
						
						$GHDLParameters = @("-a", "-C", "--std=87", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.v87")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
				
				$VHDLSrcLibrary = "vital95"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v87\$SourceFile.v87"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=87", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
					
				$VHDLSrcLibrary = "synopsys"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v87\$SourceFile.v87"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=87", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
			}
			
# ==============================================================================
# v93
# ==============================================================================
		# create 'v93' directory if it does not exist
		$LocalDirName = "v93"
		$LocalDir = $VHDLDestLibraryDir + "\" + $LocalDirName
		if (Test-Path -Path $LocalDir)
			{	Write-Host "  Directory '$LocalDirName' already exists."}
		else
			{	Write-Host "  Creating directory '$LocalDirName'."
				[void](New-Item -ItemType directory -Path $LocalDir -ErrorAction SilentlyContinue)
			}
		
		Write-Host "  compiling into $LocalDirName"
		
		# ----------------------------------------------------------------------
		# v93\std
		# ----------------------------------------------------------------------
		if ($Script_ExitCode -eq 0)
			{	$VHDLDestLibrary = "std"
				
				# create 'std' directory if it does not exist
				$LocalDir2Name = $VHDLDestLibrary
				$LocalDir2 = $LocalDir + "\" + $LocalDir2Name
				
				Write-Host "    compiling library $VHDLDestLibrary" -ForegroundColor DarkCyan
				if (Test-Path -Path $LocalDir2)
					{		if ($Script_EnableVerbose) {	Write-Host "      Directory '$LocalDir2Name' already exists."		}	}
				else
					{	Write-Host "      Creating directory '$LocalDir2Name'."
						[void](New-Item -ItemType directory -Path $LocalDir2 -ErrorAction SilentlyContinue)
					}

				# change working directory to LocalDir2
				Write-Host "      cd $LocalDir2"
				Set-Location $LocalDir2

				$VHDLSrcLibrary = "std"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        ghdlfilter (-v93)"	}
						Get-Content "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" -Encoding Ascii -Raw `
							| & $GHDLFilterExecutable @('-v93') `
							| Out-File "$SourceFile.v93" -Encoding Ascii
						
						$GHDLParameters = @("-a", "-C", "--std=93", "--bootstrap", "--work=$VHDLDestLibrary", "$SourceFile.v93")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
			}
			
		# ----------------------------------------------------------------------
		# v93\ieee
		# ----------------------------------------------------------------------
		if ($Script_ExitCode -eq 0)
			{	$VHDLDestLibrary = "ieee"
				
				# create 'ieee' directory if it does not exist
				$LocalDir2Name = $VHDLDestLibrary
				$LocalDir2 = $LocalDir + "\" + $LocalDir2Name
				
				Write-Host "    compiling library $VHDLDestLibrary" -ForegroundColor DarkCyan
				if (Test-Path -Path $LocalDir2)
					{		if ($Script_EnableVerbose) {	Write-Host "      Directory '$LocalDir2Name' already exists."		}	}
				else
					{	Write-Host "      Creating directory '$LocalDir2Name'."
						[void](New-Item -ItemType directory -Path $LocalDir2 -ErrorAction SilentlyContinue)
					}

				# change working directory to LocalDir2
				Write-Host "      cd $LocalDir2"
				Set-Location $LocalDir2

				$VHDLSrcLibrary = "ieee"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        ghdlfilter (-v93)"	}
						Get-Content "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" -Encoding Ascii -Raw `
							| & $GHDLFilterExecutable @('-v93') `
							| Out-File "$SourceFile.v93" -Encoding Ascii
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.v93")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
					
				$VHDLSrcLibrary = "ieee"
				foreach ($SourceFile in $SourceFiles['math'])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
				
				$VHDLSrcLibrary = "vital2000"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
			}
		
		# ----------------------------------------------------------------------
		# v93\synopsys
		# ----------------------------------------------------------------------
		if ($Script_ExitCode -eq 0)
			{	$VHDLDestLibrary = "ieee"
				
				# create 'synopsys' directory if it does not exist
				$LocalDir2Name = "synopsys"
				$LocalDir2 = $LocalDir + "\" + $LocalDir2Name
				
				Write-Host "    compiling library $VHDLDestLibrary" -ForegroundColor DarkCyan
				if (Test-Path -Path $LocalDir2)
					{		if ($Script_EnableVerbose) {	Write-Host "      Directory '$LocalDir2Name' already exists."		}	}
				else
					{	Write-Host "      Creating directory '$LocalDir2Name'."
						[void](New-Item -ItemType directory -Path $LocalDir2 -ErrorAction SilentlyContinue)
					}

				# change working directory to LocalDir2
				Write-Host "      cd $LocalDir2"
				Set-Location $LocalDir2

				$VHDLSrcLibrary = "ieee"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        ghdlfilter (-v93)"	}
						Get-Content "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" -Encoding Ascii -Raw `
							| & $GHDLFilterExecutable @('-v93') `
							| Out-File "$SourceFile.v93" -Encoding Ascii
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.v93")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
				
				$VHDLSrcLibrary = "ieee"
				foreach ($SourceFile in $SourceFiles['math'])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
				
				$VHDLSrcLibrary = "vital2000"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
					
				$VHDLSrcLibrary = "synopsys"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
			}
		
		# ----------------------------------------------------------------------
		# v93\mentor
		# ----------------------------------------------------------------------
		if ($Script_ExitCode -eq 0)
			{	$VHDLDestLibrary = "ieee"
				
				# create 'mentor' directory if it does not exist
				$LocalDir2Name = "mentor"
				$LocalDir2 = $LocalDir + "\" + $LocalDir2Name
				
				Write-Host "    compiling library $VHDLDestLibrary" -ForegroundColor DarkCyan
				if (Test-Path -Path $LocalDir2)
					{		if ($Script_EnableVerbose) {	Write-Host "      Directory '$LocalDir2Name' already exists."		}	}
				else
					{	Write-Host "      Creating directory '$LocalDir2Name'."
						[void](New-Item -ItemType directory -Path $LocalDir2 -ErrorAction SilentlyContinue)
					}

				# change working directory to LocalDir2
				Write-Host "      cd $LocalDir2"
				Set-Location $LocalDir2

				$VHDLSrcLibrary = "ieee"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        ghdlfilter (-v93)"	}
						Get-Content "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" -Encoding Ascii -Raw `
							| & $GHDLFilterExecutable @('-v93') `
							| Out-File "$SourceFile.v93" -Encoding Ascii
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.v93")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
				
				$VHDLSrcLibrary = "ieee"
				foreach ($SourceFile in $SourceFiles['math'])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
				
				$VHDLSrcLibrary = "vital2000"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
					
				$VHDLSrcLibrary = "mentor"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v93\$SourceFile.v93"
						if ($Script_EnableVerbose) {	Write-Host "        copy: $SourceFile"	}
						Copy-Item "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" "$SourceFile.vhd"
						
						$GHDLParameters = @("-a", "-C", "--std=93", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.vhd")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
			}

# ==============================================================================
# v08
# ==============================================================================
		# create 'v08' directory if it does not exist
		$LocalDirName = "v08"
		$LocalDir = $VHDLDestLibraryDir + "\" + $LocalDirName
		if (Test-Path -Path $LocalDir)
			{	Write-Host "  Directory '$LocalDirName' already exists."}
		else
			{	Write-Host "  Creating directory '$LocalDirName'."
				[void](New-Item -ItemType directory -Path $LocalDir -ErrorAction SilentlyContinue)
			}
		
		Write-Host "  compiling into $LocalDirName"
		
		# ----------------------------------------------------------------------
		# v08\std
		# ----------------------------------------------------------------------
		if ($Script_ExitCode -eq 0)
			{	$VHDLDestLibrary = "std"
				
				# create 'std' directory if it does not exist
				$LocalDir2Name = $VHDLDestLibrary
				$LocalDir2 = $LocalDir + "\" + $LocalDir2Name
				
				Write-Host "    compiling library $VHDLDestLibrary" -ForegroundColor DarkCyan
				if (Test-Path -Path $LocalDir2)
					{		if ($Script_EnableVerbose) {	Write-Host "      Directory '$LocalDir2Name' already exists."		}	}
				else
					{	Write-Host "      Creating directory '$LocalDir2Name'."
						[void](New-Item -ItemType directory -Path $LocalDir2 -ErrorAction SilentlyContinue)
					}

				# change working directory to LocalDir2
				Write-Host "      cd $LocalDir2"
				Set-Location $LocalDir2

				$VHDLSrcLibrary = "std"
				$VHDLSrcLibraryFiles = "std08"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibraryFiles])
					{	Write-Host "      file: v08\$SourceFile.v08"
						if ($Script_EnableVerbose) {	Write-Host "        ghdlfilter (-v08)"	}
						Get-Content "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" -Encoding Ascii -Raw `
							| & $GHDLFilterExecutable @('-v08') `
							| Out-File "$SourceFile.v08" -Encoding Ascii
						
						$GHDLParameters = @("-a", "-C", "--std=08", "--bootstrap", "--work=$VHDLDestLibrary", "$SourceFile.v08")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
			}
			
		# ----------------------------------------------------------------------
		# v08\ieee
		# ----------------------------------------------------------------------
		if ($Script_ExitCode -eq 0)
			{	$VHDLDestLibrary = "ieee"
				
				# create 'ieee' directory if it does not exist
				$LocalDir2Name = $VHDLDestLibrary
				$LocalDir2 = $LocalDir + "\" + $LocalDir2Name
				
				Write-Host "    compiling library $VHDLDestLibrary" -ForegroundColor DarkCyan
				if (Test-Path -Path $LocalDir2)
					{		if ($Script_EnableVerbose) {	Write-Host "      Directory '$LocalDir2Name' already exists."		}	}
				else
					{	Write-Host "      Creating directory '$LocalDir2Name'."
						[void](New-Item -ItemType directory -Path $LocalDir2 -ErrorAction SilentlyContinue)
					}

				# change working directory to LocalDir2
				Write-Host "      cd $LocalDir2"
				Set-Location $LocalDir2

				$VHDLSrcLibrary = "ieee2008"
				foreach ($SourceFile in $SourceFiles[$VHDLSrcLibrary])
					{	Write-Host "      file: v08\$SourceFile.v08"
						if ($Script_EnableVerbose) {	Write-Host "        ghdlfilter (-v08)"	}
						Get-Content "$VHDLSourceLibraryDir\$VHDLSrcLibrary\$SourceFile.vhdl" -Encoding Ascii -Raw `
							| & $GHDLFilterExecutable @('-v08') `
							| Out-File "$SourceFile.v08" -Encoding Ascii
						
						$GHDLParameters = @("-a", "-C", "--std=08", "-P..\std", "--work=$VHDLDestLibrary", "$SourceFile.v08")
						if ($Script_EnableVerbose) {	Write-Host ("        ghdl analyse (" + ($GHDLParameters -join " ") + ")")	}
						& $GHDLExecutable $GHDLParameters
					}
			}
# ==============================================================================
# vXX
# ==============================================================================
	} # Compile
else
	{	Write-Host "ERROR: missing argument(s)" -ForegroundColor Red
		Write-Host
		Write-Host "Usage:"
		Write-Host "  complib.ps1 [-Verbose] [-Debug] (-Help|-Compile|-Clean)" -ForegroundColor Gray
		Write-Host
	} # Unknown

# restore working directory if changed
Set-Location $Script_WorkingDir

# return exit status
exit $Script_ExitCode