# EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
# Authors:            Patrick Lehmann  (ported batch file to PowerShell)
#                     Brian Davis      (contributions to the batch file)
#                     Tristan Gingold  (initial batch file for compilations on Windows)
# 
# PowerShell Script:  Script to compile GHDL for Windows
# 
# Description:
# ------------------------------------
# This is a PowerShell script (executable) which:
#   - compiles GHDL and GHDLFilter
#   - analyses VHDL libraries
#   - installs GHDL into a directory (xcopy deploiment)
#
# ==============================================================================
#	Copyright (C) 2015-2018 Patrick Lehmann - Boetzingen, Germany
# Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
#  
# GHDL is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free
# Software Foundation; either version 2, or (at your option) any later
# version.
# 
# GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with GHDL; see the file COPYING.  If not, write to the Free
# Software Foundation, 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.
# ==============================================================================

# .SYNOPSIS 
# GHDL for Windows - GHDL compile script
# Use 'compile.ps1 -Help' to see the integrated help page
# 
# .EXAMPLE
# #
# # Normal flow
# PS> .\compile.ps1 -Clean
# PS> .\compile.ps1 -Compile
# PS> .\compile.ps1 -Install
# 
# # Combine all commands in a single call
# PS>.\compile.ps1 -Clean -Compile -Install "C:\Tools\GHDL"
#
# # Install to user defined dir
# PS> .\compile.ps1 -Install "C:\Tools\GHDL"
#
# # Update or Uninstall
# PS> .\compile.ps1 -Update
# PS> .\compile.ps1 -Uninstall
# 
# # Create a Zip-file
# PS>.\compile.ps1 -Package -Zip 
#
[CmdletBinding()]
Param(
	# Compile, Test and Package
	[switch]$All =							  $false,

	# Clean up all files and directories
	[switch]$Clean =              $false,
		[switch]$CleanGHDL =        $false,
		[switch]$CleanLibraries =   $false,
		[switch]$CleanPackages =    $false,
	
	# Compile GHDL and libraries
	[switch]$Compile =            $false,
		# Compile only GHDL
		[switch]$CompileGHDL =      $false,
		# Compile only IEEE libraries
		[switch]$CompileLibraries = $false,
			# Set VHDL Standard to '87
			[switch]$VHDL87 =         $false,
			# Set VHDL Standard to '93
			[switch]$VHDL93 =         $false,
			# Set VHDL Standard to '08
			[switch]$VHDL2008 =       $false,
		# Compile in release mode (no Git hash)
		[switch]$Release =          $false,

	# Undocumented
	[switch]$Test =               $false,
		# Undocumented
		[switch]$TestGHDL =         $false,
		
	# Create an installer package
	[switch]$Package =            $false,
		# Creates a zip-file for xcopy deployment
		[switch]$Zip =              $false,
		# Creates a self-extracting ps1-file for xcopy deployment
		[switch]$PS1 =              $false,
	
	# Install all files into a directory (xcopy deployment)
	[switch]$Install =            $false,
	# Installation directory
	[parameter(mandatory=$false, ValueFromRemainingArguments=$true)]
	[string]$InstallDir =         "",
	# Update files
	[switch]$Update =             $false,
	# Uninstall all files from a directory
	[switch]$Uninstall =          $false,

	# register GHDL in PATH
	[Parameter(Mandatory=$false)]
	[ValidateSet("Machine", "User", "Session", "Remove", "Pass")]
	[String]$AddToPath =          "",
	
	# Display this help"
	[switch]$Help =               $false,
	
	# Reduced messages
	[switch]$Quiet =						  $false,
	# Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings =   $false,
	# Halt on errors
	[switch]$HaltOnError =			  $false
)

# configure script here
$RelPathToRoot =      "..\.."

# save parameters and current working directory
$Script_ScriptDir =    $PSScriptRoot
$Script_WorkingDir =  Get-Location
$GHDLRootDir =        Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $RelPathToRoot))

# set default values
$EnableDebug =        [bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
$EnableVerbose =      [bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug

# load modules from GHDL's 'libraries' directory
Import-Module $PSScriptRoot\shared.psm1   -Verbose:$false -Debug:$false -ArgumentList "$Script_WorkingDir", $true
Import-Module $PSScriptRoot\targets.psm1  -Verbose:$false -Debug:$false

# define a function to exit script and unload imported modules
function Exit-Script
{	[CmdletBinding()]
	param(
		[int]$ExitCode = 0
	)
	cd $Script_WorkingDir
	# unload modules
	Remove-Module shared  -Verbose:$false -Debug:$false
	Remove-Module targets -Verbose:$false -Debug:$false
	exit $ExitCode
}

# Display help if no command was selected
$Help = $Help -or (-not (
					$All -or 
					$Clean -or $CleanGHDL -or $CleanLibraries -or $CleanPackages -or
					$Compile -or $CompileGHDL -or $CompileLibraries -or
					$Test -or $TestGHDL -or
					$Package -or
					$Install -or $Update -or $Uninstall
				))

$Git_IsGitRepo =						Test-GitRepository
# gather git information
if ($Git_IsGitRepo)
{	$Git_Branch_Name =				& git rev-parse --abbrev-ref HEAD
	$Git_Commit_DateString =	& git log -1 --format=%cd --date=short
	$Git_Commit_ShortHash =		& git rev-parse --short HEAD
}

Write-Host "================================================================================" -ForegroundColor Magenta
Write-Host "GHDL for Windows - GHDL compile and bundle script" -ForegroundColor Magenta
Write-Host "================================================================================" -ForegroundColor Magenta
Write-Host "  Version:    $GHDLVersion"
Write-Host "  Release:    $BuildRelease"
if ($Git_IsGitRepo)
{	Write-Host "  Git branch: $Git_Branch_Name"
	Write-Host "  Git commit: $Git_Commit_DateString ($Git_Commit_ShortHash)"
}

Write-Host

if ([environment]::OSVersion.Platform -ne "WIN32NT")
{	Write-Error "[ERROR] PowerShell based installer not supported on Linux/Unix/MacOS platforms."
	Exit-Script -1
}

if ($Help)
{	Get-Help $MYINVOCATION.InvocationName -Detailed
	Exit-Script
}

if ($All)
{	$Clean =              $true
	$Compile =            $true
	$Test =               $true
	$Package =            $true
}
if ($Clean)
{	$CleanGHDL =          $true
	$CleanLibraries =     $true
	$CleanPackages =      $true
}
if ($CleanPackages)
{	if (-not ($Zip -or $PS1))
	{	$CleanPackageZip =  $true
		$CleanPackagePS1 =  $true
	}
	if ($Zip)
	{ $CleanPackageZip =  $true }
	if ($PS1)
	{ $CleanPackagePS1 =  $true }
}
if ($Compile)
{	$CompileGHDL =        $true
	$CompileLibraries =   $true
}
if ($CompileLibraries)
{	if (-not($VHDL87 -or $VHDL93 -or $VHDL2008))
	{	$CompileLibraryVHDL87 = $true
		$CompileLibraryVHDL93 = $true
		$CompileLibraryVHDL08 = $true
	}
	if ($VHDL87)
	{	$CompileLibraryVHDL87 = $true }
	if ($VHDL93)
	{	$CompileLibraryVHDL93 = $true }
	if ($VHDL2008)
	{	$CompileLibraryVHDL08 = $true }
}
if ($Test)
{	$TestGHDL =           $true
}
if ($Package)
{	$PS1 =                $true
	$ZIP =                $true
}
if ($Install)
{
}
if ($Update)
{
}
if ($Uninstall)
{
}

try
{	$GHDLVersion =              Get-GHDLVersion $GHDLRootDir
}
catch
{	$GHDLVersion =              "unknown"
}

# configure some variables: paths, executables, directory names, ...
$Backend =                               "mcode"
$Flavors =                               @("synopsys", "mentor")
$WindowsDirName =                        "dist\windows"    #\$Backend"
$BuildDirectoryName =                    "build"
$BuildBackendDirectoryName =             "$BuildDirectoryName\$Backend"
$VHDLLibrariesSourceDirectoryName =      "libraries"
$VHDLLibrariesDestinationDirectoryName = "lib"
$PackageDirectoryName =                  "build\zip\$Backend"
$ZipPackageFileName =                    "ghdl-$Backend-$GHDLVersion.zip"
$PS1PackageFileName =                    "ghdl-$Backend-$GHDLVersion.installer.ps1"
$InstallerTemplateFileName =             "InstallerTemplate.ps1"
$DefaultInstallPath =                    "C:\Program Files (x86)\GHDL"        # This is the default path for 32-bit applications (x86-32)

# construct directories
$GHDLWindowsDir =                   "$GHDLRootDir\$WindowsDirName"
$GHDLBuildDir =                     "$GHDLRootDir\$BuildBackendDirectoryName"
$BinaryDestinationDirectory =       "$GHDLBuildDir"
$VHDLLibrarySourceDirectory =       "$GHDLRootDir\$VHDLLibrariesSourceDirectoryName"
$GHDLVendorLibraryDir =             "$GHDLRootDir\$VHDLLibrariesSourceDirectoryName\vendors"
$VHDLLibraryDestinationDirectory =  "$GHDLBuildDir\$VHDLLibrariesDestinationDirectoryName"
$GHDLCompiledLibraryDir =           "$VHDLLibraryDestinationDirectory"
$GHDLZipPackageDir =                "$GHDLRootDir\$PackageDirectoryName"
$GHDLZipPackageFile =               "$GHDLZipPackageDir\$ZipPackageFileName"
$InstallerTemplateFile =            "$GHDLWindowsDir\$InstallerTemplateFileName"
$GHDLPS1PackageFile =               "$GHDLZipPackageDir\$PS1PackageFileName"

if ($Release)
{	$BuildRelease =             "Release"
	$BuildDirectory =           $BinaryDestinationDirectory
}
else
{	$BuildRelease =             "Development"
	$BuildDirectory =           $BinaryDestinationDirectory
}

# construct executables
$GHDLNewExecutable =					"$GHDLBuildDir\bin\ghdl.exe"

# construct files
$InstallDirFile =             "$BuildDirectoryName\InstallDir.conf"

$EnvPath_ContainerMapping = @{
	Machine = [EnvironmentVariableTarget]::Machine
	User =    [EnvironmentVariableTarget]::User
}


if ($Uninstall)
{	Write-Host "Uninstalling GHDL for Windows..."

	Write-Host "[ERROR] This command is not implemented." -ForegroundColor Red
	Exit-Script -1
}  # Uninstall

# ==============================================================================
# Clean tasks
# ==============================================================================
if ($Clean)
{	Write-Host "Cleaning all created files and directories..." -ForegroundColor Cyan  }

# Clean GHDL
# ------------------------------------------------------------------------------
if ($CleanGHDL)
{	$Clean = $true
	try
	{	Invoke-CleanGHDL $BuildDirectory -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}
}  # CleanGHDL

# Clean Libraries
# ------------------------------------------------------------------------------
if ($CleanLibraries)
{	$Clean = $true
	try
	{	if ($CleanGHDL)  { Write-Host }
		Invoke-CleanLibraries $VHDLLibraryDestinationDirectory -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}
}  # CleanLibraries

# Clean Packages
# ------------------------------------------------------------------------------
if ($CleanPackages)
{	$Clean = $true
	try
	{	if ($CleanGHDL -or $CleanLibraries) { Write-Host }
		if ($CleanPackageZip)
		{	Invoke-CleanPackageZip $GHDLZipPackageDir $GHDLZipPackageFile -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
		if ($CleanPackagePS1)
		{	if ($CleanPackageZip) { Write-Host }
			Invoke-CleanPackagePS1 $GHDLPS1PackageFile -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug
		}
	}
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}
}  # CleanPackages
if ($Clean)
{	Write-Host    
	Write-Host "Clean " -NoNewline
	Write-Host "[SUCCESSFUL]" -ForegroundColor Green
}

# ==============================================================================
# Compile tasks
# ==============================================================================
if ($Compile)
{	if ($Clean) { Write-Host  }
	Write-Host "Compiling GHDL and Libraries..." -ForegroundColor Cyan
}

# Compile GHDL
# ------------------------------------------------------------------------------
if ($CompileGHDL)
{	$Compile = $true
	if ($Clean -and -not $Compile)
	{	Write-Host    }
	
	# create a build directory
	try
	{	New-BuildDirectory $BuildDirectory -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}
	
	# patch the version file if it's no release build
	if (-not $Release -and $Git_IsGitRepo)
	{	try
		{	Invoke-PatchVersionFile $GHDLRootDir $Git_Branch_Name $Git_Commit_DateString $Git_Commit_ShortHash -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
		catch
		{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
			Exit-Script -1
		}
	}
	
	# build C source files
	try
	{	Write-Host
		Invoke-CompileCFiles $GHDLRootDir $BinaryDestinationDirectory -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}

	
	# build Ada source files
	try
	{	Write-Host
		Invoke-CompileGHDLAdaFiles $GHDLRootDir $BinaryDestinationDirectory -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}
	
	# strip result
	try
	{	Write-Host
		Invoke-StripGHDLExecutable $BinaryDestinationDirectory -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}
	
	Write-Host    
	Write-Host "Compile GHDL " -NoNewline
	Write-Host "[SUCCESSFUL]" -ForegroundColor Green
}  # CompileGHDL



# Compile Libraries
# ------------------------------------------------------------------------------
if ($CompileLibraries)
{	if (($Clean -and -not $Compile) -or $CompileGHDL)  { Write-Host }
	Write-Host "Compiling all VHDL libraries..." -ForegroundColor Cyan
}

if ($CompileLibraryVHDL87 -or $CompileLibraryVHDL93 -or $CompileLibraryVHDL08)
{	$env:GHDL = "$GHDLBuildDir"
	Write-Host ("  Setting `$env:GHDL to '" + $env:GHDL + "'")

	$EnableDebug -and (Write-Host "$Indentation    Import-Module ghdl ..." -ForegroundColor Yellow) | Out-Null
	Import-Module $PSScriptRoot\ghdl.psm1 -Verbose:$false -Debug:$false -ArgumentList "", $Script_WorkingDir 3>$null
	
	try
	{	Invoke-PrepareCompileLibrary $VHDLLibraryDestinationDirectory -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}
	
	Write-Host
	Write-Host "Start compilation..."
}

if ($CompileLibraryVHDL87)
{	$CompileLibraries = $true
	try
	{	Invoke-CompileLibrary $VHDLLibrarySourceDirectory $VHDLLibraryDestinationDirectory 1987 -SuppressWarnings:$SuppressWarnings -HaltOnError:$HaltOnError -Indentation:"  " -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}

	try
	{	Invoke-CompileIEEELibraryFlavor $VHDLLibrarySourceDirectory $VHDLLibraryDestinationDirectory 1987 "synopsys" -SuppressWarnings:$SuppressWarnings -HaltOnError:$HaltOnError -Indentation:"  " -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}
}  # CompileLibraryVHDL87
if ($CompileLibraryVHDL93)
{	$CompileLibraries = $true
	try
	{	Invoke-CompileLibrary $VHDLLibrarySourceDirectory $VHDLLibraryDestinationDirectory 1993 -SuppressWarnings:$SuppressWarnings -HaltOnError:$HaltOnError -Indentation:"  " -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}

	foreach ($Flavor in $Flavors)
	{	try
		{	Invoke-CompileIEEELibraryFlavor $VHDLLibrarySourceDirectory $VHDLLibraryDestinationDirectory 1993 $Flavor -SuppressWarnings:$SuppressWarnings -HaltOnError:$HaltOnError -Indentation:"  " -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
		catch
		{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
			Exit-Script -1
		}
	}
}  # CompileLibraryVHDL93
if ($CompileLibraryVHDL08)
{	$CompileLibraries = $true
	try
	{	Invoke-CompileLibrary $VHDLLibrarySourceDirectory $VHDLLibraryDestinationDirectory 2008 -SuppressWarnings:$SuppressWarnings -HaltOnError:$HaltOnError -Indentation:"  " -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}

	try
	{	Invoke-CompileIEEELibraryFlavor $VHDLLibrarySourceDirectory $VHDLLibraryDestinationDirectory 1987 "synopsys" -SuppressWarnings:$SuppressWarnings -HaltOnError:$HaltOnError -Indentation:"  " -Quiet:$Quiet -Verbose:$EnableVerbose -Debug:$EnableDebug  }
	catch
	{	Write-Host "  [ERROR] $_"	-ForegroundColor Red
		Exit-Script -1
	}
}  # CompileLibraryVHDL93
if ($CompileLibraries)
{	$EnableDebug -and (Write-Host "$Indentation    Remove-Module ghdl ..." -ForegroundColor Yellow) | Out-Null
	Remove-Module ghdl

	Write-Host    
	Write-Host "Compile Libraries " -NoNewline
	Write-Host "[SUCCESSFUL]" -ForegroundColor Green
}




# ==============================================================================
# Package tasks
# ==============================================================================
if ($Package)
{	Write-Host "Creating an installation package for GHDL $GHDLVersion for Windows"
	$Good = $false

	if ($Zip)
	{	Write-Host "Loading PowerShell Community Extensions (PSCX) " -NoNewline
		if ((Get-Module -ListAvailable | Where {$_.Name -like "PSCX"}).Version -ge "3.1.0.0")
		{	Import-Module Pscx -Verbose:$false
			Write-Host "[Done]" -ForegroundColor Green
		}
		else
		{	Write-Host "[FAILED]" -ForegroundColor RED  
			Exit-Script -1
		}
		
		Write-Host "Output format: zip-file"
		Write-Host "  Removing old directory '$GHDLZipPackageDir'."
		if (Test-Path -Path $GHDLZipPackageDir)
		{	Remove-Item $GHDLZipPackageDir -Force -Recurse -ErrorAction SilentlyContinue
			if ($? -eq $false)
			{	Write-Host "[ERROR]: While deleting '$GHDLZipPackageDir'." -ForegroundColor Red
				Exit-Script -1
			}
		}
		if (Test-Path -Path $GHDLZipPackageFile)
		{	Remove-Item $GHDLZipPackageFile -Force -Recurse -ErrorAction SilentlyContinue
			if ($? -eq $false)
			{	Write-Host "[ERROR]: While deleting '$GHDLZipPackageFile'." -ForegroundColor Red
				Exit-Script -1
			}
		}
	
		Write-Host "  Creating directory '$GHDLZipPackageDir' and sub-directories..."
		New-Item -ItemType directory -Path "$GHDLZipPackageDir"            -ErrorAction SilentlyContinue  | Out-Null
		New-Item -ItemType directory -Path "$GHDLZipPackageDir\bin"        -ErrorAction SilentlyContinue  | Out-Null
		New-Item -ItemType directory -Path "$GHDLZipPackageDir\include"    -ErrorAction SilentlyContinue  | Out-Null
		New-Item -ItemType directory -Path "$GHDLZipPackageDir\lib"        -ErrorAction SilentlyContinue  | Out-Null
		
		Write-Host "  Gathering files..."
		# executables
		Copy-Item "$GHDLBuildDir\ghdl.exe"            "$GHDLZipPackageDir\bin\ghdl.exe" -ErrorAction SilentlyContinue
		# include files
		Copy-Item "$GHDLRootDir\src\grt\vpi_user.h"   "$GHDLZipPackageDir\include"      -ErrorAction SilentlyContinue
		# pre-compile scripts
		Copy-Item $GHDLVendorLibraryDir -Recurse      "$GHDLZipPackageDir\lib\vendors"  -ErrorAction SilentlyContinue
		# pre-compiled libraries
		Copy-Item $GHDLCompiledLibraryDir -Recurse    "$GHDLZipPackageDir"              -ErrorAction SilentlyContinue

		Write-Host "  Compressing all files into '$GHDLZipPackageFile'..."
		$file = Get-ChildItem $GHDLZipPackageDir -Recurse | Write-Zip -IncludeEmptyDirectories -EntryPathRoot $GHDLZipPackageDir -OutputPath $GHDLZipPackageFile
		Write-Host "  $([math]::round(($file.Length / 1MB), 3)) MiB written to disk"
		 
		 Write-Host
		Write-Host "Creating package " -NoNewline
		Write-Host "[SUCCESSFUL]" -ForegroundColor Green
		Write-Host
		
		$Good = $true
	}
	
	if ($PS1)
	{	Write-Host "Creating a self-extracting PowerShell package for GHDL $GHDLVersion for Windows"
	
		if (-not (Test-Path -Path $GHDLZipPackageFile))
		{	Write-Host "[ERROR]: ZIP file '$GHDLZipPackageFile' does not exist." -ForegroundColor Red
			Exit-Script -1
		}
	
		# Read ZIP file and convert it to base64
		$ResolvedPath =                   Resolve-Path "$GHDLZipPackageFile"
		$CompressedFileContentAsBytes =   [System.IO.File]::ReadAllBytes("$ResolvedPath")
		$CompressedFileContentInBase64 =  [System.Convert]::ToBase64String($CompressedFileContentAsBytes)
		
		# Read a Installer template and add the base64 content
		$Installer = Get-Content $InstallerTemplateFile
		$Installer = $Installer -replace "# DATASECTION", "`$CompressedFileContentInBase64 = `"$CompressedFileContentInBase64`""
		$Installer | Out-File -FilePath $GHDLPS1PackageFile
				
		Write-Host
		Write-Host "Creating package " -NoNewline
		Write-Host "[SUCCESSFUL]" -ForegroundColor Green
		Write-Host
		
		$Good = $true
	}
	
	if (-not $Good)
	{	Write-Host "[ERROR]: No package format selected." -ForegroundColor Red
		Write-Host "Possible formats:"
		Write-Host "  - Zip-file (-Zip)"
		Write-Host
	}
}
# ============================================================================
# Install tasks
# ============================================================================
if ($Install)
{	Write-Host "Installing GHDL $GHDLVersion for Windows..."
	if ($InstallDir -eq "")
	{	if (Test-Path $InstallDirFile -PathType Leaf)
		{	Write-Host "  Reading installation path from '$InstallDirFile' ..."
			$InstallPath = Get-Content $InstallDirFile -Encoding Ascii
		}
		else
		{	$InstallPath = $DefaultInstallPath  }
	}
	else
	{	$InstallPath = $InstallDir      }
	$InstallPath = $InstallPath.TrimEnd("\")
	
	if ($Zip)
	{	Write-Host "Loading PowerShell Community Extensions (PSCX) " -NoNewline
		if ((Get-Module -ListAvailable | Where {$_.Name -like "PSCX"}).Version -ge "3.1.0.0")
		{	Import-Module Pscx -Verbose:$false
			Write-Host "[Done]" -ForegroundColor Green
		}
		else
		{	Write-Host "[FAILED]" -ForegroundColor RED  
			Exit-Script -1
		}
		
		Write-Host "  Installing from Zip-file..."
		
		Write-Host "[ERROR]: This command is not implemented." -ForegroundColor Red
	}
	else
	{	Write-Host "  Writing installation path to '$InstallDirFile'..."
		$InstallPath | Out-File -FilePath $InstallDirFile -Encoding Ascii
	
		if (Test-Path -Path $InstallPath)
		{	Write-Host "[ERROR]: Directory '$InstallPath' already exists." -ForegroundColor Red
			Exit-Script -1
		}
		Write-Host "  Install directory: $InstallPath"
		Write-Host "  Creating directory '$InstallPath' and sub-directories..."
		New-Item -ItemType directory -Path "$InstallPath"            -ErrorAction SilentlyContinue  | Out-Null
		New-Item -ItemType directory -Path "$InstallPath\bin"        -ErrorAction SilentlyContinue  | Out-Null
		New-Item -ItemType directory -Path "$InstallPath\include"    -ErrorAction SilentlyContinue  | Out-Null
		New-Item -ItemType directory -Path "$InstallPath\lib"        -ErrorAction SilentlyContinue  | Out-Null
		
		Write-Host "  Copying files..."
		# executables
		Copy-Item "$GHDLBuildDir\ghdl.exe"            "$InstallPath\bin\ghdl.exe" -Verbose:$EnableVerbose -ErrorAction SilentlyContinue
		# include files
		Copy-Item "$GHDLRootDir\src\grt\vpi_user.h"   "$InstallPath\include"      -Verbose:$EnableVerbose -ErrorAction SilentlyContinue
		# pre-compile scripts
		Copy-Item $GHDLVendorLibraryDir -Recurse      "$InstallPath\lib"          -Verbose:$EnableVerbose -ErrorAction SilentlyContinue
		# pre-compiled libraries
		Copy-Item $GHDLCompiledLibraryDir  -Recurse   "$InstallPath"              -Verbose:$EnableVerbose -ErrorAction SilentlyContinue

		while($true)
		{	Write-Host "  Install GHDL in PATH at machine level? [" -NoNewline -ForegroundColor DarkCyan
			Write-Host "M" -NoNewline -ForegroundColor Cyan
			Write-Host "achine/" -NoNewline -ForegroundColor DarkCyan
			Write-Host "u" -NoNewline -ForegroundColor Cyan
			Write-Host "ser/" -NoNewline -ForegroundColor DarkCyan
			Write-Host "s" -NoNewline -ForegroundColor Cyan
			Write-Host "ession/" -NoNewline -ForegroundColor DarkCyan
			Write-Host "p" -NoNewline -ForegroundColor Cyan
			Write-Host "ass]: " -NoNewline -ForegroundColor DarkCyan
			$InstallInPath = (Read-Host).ToLower()
			if ($InstallInPath -in "m","u","s","p")
			{	break	}
			else
			{	Write-Host "[ERROR]: Unsupported choice: '$InstallInPath'." -ForegroundColor Red    }
		}
		
		if (($InstallInPath -eq "") -or ($InstallInPath -eq "m"))
		{	Write-Host "  Adding GHDL to PATH at machine level."
			Add-EnvPath -Path "$InstallPath\bin" -Container "Machine"
			Add-EnvPath -Path "$InstallPath\bin" -Container "Session"
		}
		elseif ($InstallInPath -eq "u")
		{	Write-Host "  Adding GHDL to PATH at user level."
			Add-EnvPath -Path "$InstallPath\bin" -Container "User"
			Add-EnvPath -Path "$InstallPath\bin" -Container "Session"
		}
		elseif ($InstallInPath -eq "s")
		{	Write-Host "  Adding GHDL to PATH at session level."
			Add-EnvPath -Path "$InstallPath\bin" -Container "Session"
		}
		
		Write-Host
		Write-Host "Installing files " -NoNewline
		Write-Host "[SUCCESSFUL]" -ForegroundColor Green
		Write-Host
		
		Exit-Script
	}  # Zip
}  # Install
elseif ($Update)
{	Write-Host "Updating GHDL $GHDLVersion for Windows..."
	if (Test-Path $InstallDirFile -PathType Leaf)
	{	Write-Host "  Reading installation path from '$InstallDirFile' ..."
		$InstallPath = Get-Content $InstallDirFile -Encoding Ascii
	}
	else
	{	if ($InstallDir -eq "")
		{	   }
		else
		{	$InstallPath = $InstallDir      }
	}
	$InstallPath = $InstallPath.TrimEnd("\")
	
	Write-Host "  Install directory: $InstallPath"
	if (Test-Path -Path $InstallPath)
	{	Write-Host "  Cleaning up installation directory '$InstallPath'." -ForegroundColor Yellow
		Get-ChildItem -Path $InstallPath -Depth 0 | foreach { Remove-Item $_.FullName -Recurse -Force }
	}
	
	
	Write-Host "  Creating directory sub-directories in '$InstallPath' ..."
	New-Item -ItemType directory -Path "$InstallPath\bin"        -ErrorAction SilentlyContinue  | Out-Null
	New-Item -ItemType directory -Path "$InstallPath\include"    -ErrorAction SilentlyContinue  | Out-Null
	New-Item -ItemType directory -Path "$InstallPath\lib"        -ErrorAction SilentlyContinue  | Out-Null
	
	Write-Host "  Copying files..."
	# executables
	Copy-Item "$GHDLBuildDir\ghdl.exe"            "$InstallPath\bin\ghdl.exe" -Verbose:$EnableVerbose -ErrorAction SilentlyContinue
	# include files
	Copy-Item "$GHDLRootDir\src\grt\vpi_user.h"   "$InstallPath\include"      -Verbose:$EnableVerbose -ErrorAction SilentlyContinue
	# pre-compile scripts
	Copy-Item $GHDLVendorLibraryDir -Recurse      "$InstallPath\lib"          -Verbose:$EnableVerbose -ErrorAction SilentlyContinue
	# pre-compiled libraries
	Copy-Item $GHDLCompiledLibraryDir  -Recurse   "$InstallPath"              -Verbose:$EnableVerbose -ErrorAction SilentlyContinue

	if ($AddToPath -eq "")
	{	while($true)
		{	Write-Host "  Install GHDL in PATH at machine level? [" -NoNewline -ForegroundColor DarkCyan
			Write-Host "M" -NoNewline -ForegroundColor Cyan
			Write-Host "achine/" -NoNewline -ForegroundColor DarkCyan
			Write-Host "u" -NoNewline -ForegroundColor Cyan
			Write-Host "ser/" -NoNewline -ForegroundColor DarkCyan
			Write-Host "s" -NoNewline -ForegroundColor Cyan
			Write-Host "ession/" -NoNewline -ForegroundColor DarkCyan
			Write-Host "r" -NoNewline -ForegroundColor Cyan
			Write-Host "emove/" -NoNewline -ForegroundColor DarkCyan
			Write-Host "p" -NoNewline -ForegroundColor Cyan
			Write-Host "ass]: " -NoNewline -ForegroundColor DarkCyan
			$InstallInPath = (Read-Host).ToLower()
			if ($InstallInPath -in "m","u","s","r","p")
			{	break	}
			else
			{	Write-Host "[ERROR]: Unsupported choice: '$InstallInPath'." -ForegroundColor Red    }
		}
	}
	elseif ($AddToPath -eq "Machine")
	{	$InstallInPath = "m"     }
	elseif ($AddToPath -eq "User")
	{	$InstallInPath = "u"     }
	elseif ($AddToPath -eq "Session")
	{	$InstallInPath = "s"     }
	elseif ($AddToPath -eq "Remove")
	{	$InstallInPath = "r"     }
	elseif ($AddToPath -eq "Pass")
	{	$InstallInPath = "p"     }

	if ($InstallInPath -ne "p")
	{	Write-Host "  Removing GHDL from PATH variables in Machine, User, Session ..." -ForegroundColor Yellow
		foreach ($container in @("Machine", "User"))
		{	foreach ($entry in (Get-EnvPath -Container $container))
			{	if ($entry.ToLower().Contains("ghdl"))
				{	Write-Host "    Removing '$entry' from $container level."
					Remove-EnvPath -Path $entry -Container $container
				}
			}
		}
		Remove-EnvPath -Path $entry -Container "Session"
		
		if (($InstallInPath -eq "") -or ($InstallInPath -eq "m"))
		{	Write-Host "  Adding GHDL to PATH at machine level."
			Add-EnvPath -Path "$InstallPath\bin" -Container "Machine"
			Add-EnvPath -Path "$InstallPath\bin" -Container "Session"
		}
		elseif ($InstallInPath -eq "u")
		{	Write-Host "  Adding GHDL to PATH at user level."
			Add-EnvPath -Path "$InstallPath\bin" -Container "User"
			Add-EnvPath -Path "$InstallPath\bin" -Container "Session"
		}
		elseif ($InstallInPath -eq "s")
		{	Write-Host "  Adding GHDL to PATH at session level."
			Add-EnvPath -Path "$InstallPath\bin" -Container "Session"
		}
	}
	
	Write-Host
	Write-Host "Updating files " -NoNewline
	Write-Host "[SUCCESSFUL]" -ForegroundColor Green
	Write-Host
	
	Exit-Script
}  # Update
elseif ($Uninstall)
{	Write-Host "Uninstalling GHDL $GHDLVersion for Windows..."
	if (Test-Path $InstallDirFile -PathType Leaf)
	{	Write-Host "  Reading installation path from '$InstallDirFile' ..."
		$InstallPath = Get-Content $InstallDirFile -Encoding Ascii
	}
	else
	{	$InstallPath = $DefaultInstallPath    }
	
	Write-Host "  Install directory: $InstallPath"
	if (Test-Path -Path $InstallPath)
	{	Write-Host "  Removing installation directory '$InstallPath'." -ForegroundColor Yellow
		Remove-Item $InstallPath -Recurse -Force -ErrorAction SilentlyContinue
	}
	
	Write-Host "  Removing GHDL from PATH variables in Machine, User, Session ..." -ForegroundColor Yellow
	foreach ($container in @("Machine", "User"))
	{	foreach ($entry in (Get-EnvPath -Container $container))
		{	if ($entry.ToLower().Contains("ghdl"))
			{	Write-Host "    Removing '$entry' from $container level."
				Remove-EnvPath -Path $entry -Container $container
			}
		}
	}
	Remove-EnvPath -Path $entry -Container "Session"

	Write-Host
	Write-Host "Uninstalling files " -NoNewline
	Write-Host "[SUCCESSFUL]" -ForegroundColor Green
	Write-Host
	
	Exit-Script
}  # Uninstall
	
Exit-Script
