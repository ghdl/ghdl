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
#    - compiles GHDL and GHDLFilter
#    - analyses VHDL libraries
#    - installs GHDL into a directory (xcopy deploiment)
#
# ==============================================================================
#  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
#  Copyright (C) 2015-2017 Patrick Lehmann
#  
#  GHDL is free software; you can redistribute it and/or modify it under
#  the terms of the GNU General Public License as published by the Free
#  Software Foundation; either version 2, or (at your option) any later
#  version.
#  
#  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#  for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with GHDL; see the file COPYING.  If not, write to the Free
#  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#  02111-1307, USA.
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
	# Clean up all files and directories
	[switch]$Clean,
		[switch]$Clean_GHDL,
		[switch]$Clean_Libraries,
		
	# Compile GHDL
	[switch]$Compile,
		[switch]$Compile_GHDL,
		[switch]$Compile_Libraries,

	# Create an installer package
	[switch]$Package,
		# Creates a zip-file for xcopy deployment
		[switch]$Zip,
		# Creates a self-extracting ps1-file for xcopy deployment
		[switch]$PS1,
	
	# Install all files into a directory (xcopy deployment)
	[switch]$Install = $false,
	[parameter(mandatory=$false, ValueFromRemainingArguments=$true)]
	[string]$InstallDir = "",
	# Update files
	[switch]$Update,
	# Uninstall all files from a directory
	[switch]$Uninstall,

	# register GHDL in PATH
	[Parameter(Mandatory=$false)]
	[ValidateSet("Machine", "User", "Session", "Remove", "Pass")]
	[String]$AddToPath = "",
	
	# Display this help"
	[switch]$Help
)

# configure script here
$RelPathToRoot =      "..\.."

# save parameters and current working directory
$Script_ScriptDir =    $PSScriptRoot
$Script_WorkingDir =  Get-Location
$GHDLRootDir =        Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $RelPathToRoot))

# set default values
$Hosting =            $true
$EnableDebug =        [bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
$EnableVerbose =      [bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug

# load modules from GHDL's 'libraries' directory
Import-Module $PSScriptRoot\shared.psm1  -Verbose:$false -Debug:$false -ArgumentList "$Script_WorkingDir", $Hosting
Import-Module $PSScriptRoot\targets.psm1 -Verbose:$false -Debug:$false

# Display help if no command was selected
$Help = $Help -or (-not (
					$All -or 
					$Clean -or $Clean_GHDL -or $Clean_Libraries -or $Clean_Package_Zip -or
					$Compile -or $Compile_GHDL -or $Compile_Libraries -or
					$Package -or
					$Install -or $Update -or $Uninstall
				))

Write-Host "================================================================================" -ForegroundColor Magenta
Write-Host "GHDL for Windows - GHDL compile and bundle script" -ForegroundColor Magenta
Write-Host "================================================================================" -ForegroundColor Magenta

if ($Help)
{	Get-Help $MYINVOCATION.MyCommand.Path -Detailed
	Exit-CompileScript
}

if ($All)
{	$Clean =              $true
	$Compile =            $true
	$Package =            $true
}
if ($Clean)
{	$Clean_GHDL =         $true
	$Clean_Libraries =    $true
	$Clean_Package_Zip =  $true
}
if ($Compile)
{	$Compile_GHDL =       $true
	$Compile_Libraries =  $true
}

# configure some variables: paths, executables, directory names, ...
$GHDLVersion =                Get-GHDLVersion $GHDLRootDir
$Backend =                    "mcode"
$WindowsDirName =             "dist\windows"    #\$Backend"
$BuildDirectoryName =         "build"
$BuildBackendDirectoryName =  "$BuildDirectoryName\$Backend"
$VHDLLibrariesDirectoryName = "lib"
$PackageDirectoryName =       "build\zip\$Backend"
$ZipPackageFileName =         "ghdl-$Backend-$GHDLVersion.zip"
$PS1PackageFileName =         "ghdl-$Backend-$GHDLVersion.installer.ps1"
$InstallerTemplateFileName =  "InstallerTemplate.ps1"
$DefaultInstallPath =         "C:\Program Files (x86)\GHDL"        # This is the default path for 32-bit applications (x86-32)

# construct directories
$GHDLWindowsDir =             "$GHDLRootDir\$WindowsDirName"
$GHDLBuildDir =               "$GHDLRootDir\$BuildBackendDirectoryName"
$GHDLVendorLibraryDir =       "$GHDLRootDir\libraries\vendors"
$GHDLCompiledLibraryDir =     "$GHDLRootDir\$BuildBackendDirectoryName\$VHDLLibrariesDirectoryName"
$GHDLZipPackageDir =          "$GHDLRootDir\$PackageDirectoryName"
$GHDLZipPackageFile =         "$GHDLZipPackageDir\$ZipPackageFileName"
$InstallerTemplateFile =      "$GHDLWindowsDir\$InstallerTemplateFileName"
$GHDLPS1PackageFile =         "$GHDLZipPackageDir\$PS1PackageFileName"

# construct files
$InstallDirFile =             "$BuildDirectoryName\InstallDir.conf"

$EnvPath_ContainerMapping = @{
	Machine = [EnvironmentVariableTarget]::Machine
	User =    [EnvironmentVariableTarget]::User
}

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

# GitHub user:            https://github.com/mkropat
# Gist account at GitHub: https://gist.github.com/mkropat
# Gist snippet URL:       https://gist.github.com/mkropat/c1226e0cc2ca941b23a9
function Add-EnvPath
{	param(
		[Parameter(Mandatory=$true)]
		[string] $Path,

		[ValidateSet("Machine", "User", "Session")]
		[string] $Container = "Session"
	)

	if ($Container -ne "Session")
	{	$containerType =  $EnvPath_ContainerMapping[$Container]
		$persistedPaths = [Environment]::GetEnvironmentVariable("Path", $containerType) -split ";"
		if ($persistedPaths -notcontains $Path)
		{	$persistedPaths = $persistedPaths + $Path | where { $_ }
			[Environment]::SetEnvironmentVariable("Path", $persistedPaths -join ";", $containerType)
		}
	}

	$envPaths = $env:Path -split ";"
	if ($envPaths -notcontains $Path)
	{	$envPaths = $envPaths + $Path | where { $_ }
		$env:Path = $envPaths -join ";"
	}
}

# GitHub user:            https://github.com/mkropat
# Gist account at GitHub: https://gist.github.com/mkropat
# Gist snippet URL:       https://gist.github.com/mkropat/c1226e0cc2ca941b23a9
function Remove-EnvPath
{	param (
		[Parameter(Mandatory=$true)]
		[string] $Path,

		[ValidateSet("Machine", "User", "Session")]
		[string] $Container = "Session"
	)

	if ($Container -ne "Session")
	{	$containerType =  $EnvPath_ContainerMapping[$Container]
		$persistedPaths = [Environment]::GetEnvironmentVariable("Path", $containerType) -split ";"
		if ($persistedPaths -contains $Path)
		{	$persistedPaths = $persistedPaths | where { $_ -and $_ -ne $Path }
			[Environment]::SetEnvironmentVariable("Path", $persistedPaths -join ";", $containerType)
		}
	}

	$envPaths = $env:Path -split ";"
	if ($envPaths -contains $Path)
	{	$envPaths = $envPaths | where { $_ -and $_ -ne $Path }
		$env:Path = $envPaths -join ";"
	}
}

# GitHub user:            https://github.com/mkropat
# Gist account at GitHub: https://gist.github.com/mkropat
# Gist snippet URL:       https://gist.github.com/mkropat/c1226e0cc2ca941b23a9
function Get-EnvPath
{	param (
		[Parameter(Mandatory=$true)]
		[ValidateSet("Machine", "User")]
		[string] $Container
	)

	$containerType = $EnvPath_ContainerMapping[$Container]
	[Environment]::GetEnvironmentVariable('Path', $containerType) -split ";" | where { $_ }
}


if ($false)
{	# Write-Host "Uninstalling GHDL $GHDLVersion for Windows..."

	# Write-Host "[ERROR]: This command is not implemented." -ForegroundColor Red
	Exit-Script -1
}  # Uninstall
else
{	# ============================================================================
	# Clean tasks
	# ============================================================================
	if ($Clean)
	{	Write-Host "Removing all created files and directories..."    }
	
	if ($Clean_GHDL)
	{	$Script_Path =         $GHDLWindowsDir + "\compile-ghdl.ps1"
		$Script_Parameters =  @(
			'-Clean',
			'-Hosted',
			'-Verbose:$EnableVerbose',
			'-Debug:$EnableDebug'
		)
		
		Write-Host "Running compile-ghdl.ps1 -Clean ..." -ForegroundColor DarkCyan
		Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
		$InvokeExpr = "$Script_Path " + ($Script_Parameters -join " ")
		Invoke-Expression $InvokeExpr
		if ($LastExitCode -ne 0)
		{	Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
			Write-Host "[ERROR]: While executing '$InvokeExpr'." -ForegroundColor Red
			Exit-Script -1
		}
		else
		{	Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
			Write-Host "Completed compile-ghdl.ps1 " -NoNewline
			Write-Host "[SUCCESSFUL]" -ForegroundColor Green
			Write-Host
		}
	}  # Clean_GHDL
	if ($Clean_Libraries)
	{	if ($Clean_GHDL)
		{	Write-Host    }
		
		$Script_Path =        $GHDLWindowsDir + "\compile-libraries.ps1"
		$Script_Parameters =  @(
			'-Clean',
			'-Hosted',
			'-Verbose:$EnableVerbose',
			'-Debug:$EnableDebug'
		)
		
		Write-Host "Running compile-libraries.ps1 -Clean ..." -ForegroundColor DarkCyan
		Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
		$InvokeExpr = "$Script_Path " + ($Script_Parameters -join " ")
		Invoke-Expression $InvokeExpr
		if ($LastExitCode -ne 0)
		{	Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
			Write-Host "[ERROR]: While executing '$InvokeExpr'." -ForegroundColor Red
			Exit-Script -1
		}
		else
		{	Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
			Write-Host "Completed compile-libraries.ps1 " -NoNewline
			Write-Host "[SUCCESSFUL]" -ForegroundColor Green
			Write-Host
		}
	}  # Clean_Libraries
	if ($Clean_Package_Zip)
	{	if ($Clean_GHDL -or $Clean_Libraries)
		{	Write-Host    }
	
		Write-Host "Running more clean-up tasks..." -ForegroundColor DarkCyan
		Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
		Write-Host "Removing installer packages and temporary directories..." -ForegroundColor Yellow
		if (Test-Path -Path $GHDLZipPackageDir)
		{	Write-Host "  rmdir $GHDLZipPackageDir"
			Remove-Item $GHDLZipPackageDir -Force -Recurse -ErrorAction SilentlyContinue
			if ($? -eq $false)
			{	Write-Host "[ERROR]: While deleting '$GHDLZipPackageDir'." -ForegroundColor Red
				Exit-Script -1
			}
		}
		
		if (Test-Path -Path $GHDLZipPackageFile)
		{	Write-Host "  rm $GHDLZipPackageFile"
			Remove-Item $GHDLZipPackageFile -Force -Recurse -ErrorAction SilentlyContinue
			if ($? -eq $false)
			{	Write-Host "[ERROR]: While deleting '$GHDLZipPackageFile'." -ForegroundColor Red
				Exit-Script -1
			}
		}
		
		Write-Host
		Write-Host "Clean " -NoNewline
		Write-Host "[SUCCESSFUL]" -ForegroundColor Green
		Write-Host
	}  # Clean_Package_Zip
	
	# ============================================================================
	# Compile tasks
	# ============================================================================
	if ($Compile_GHDL)
	{	if ($Clean)
		{	Write-Host    }
			
		$Script_Path =        $GHDLWindowsDir + "\compile-ghdl.ps1"
		$Script_Parameters =  @()
		$Script_Parameters =  @(
			'-All',
			'-Hosted',
			'-Verbose:$EnableVerbose',
			'-Debug:$EnableDebug'
		)
		
		# Write-Host "Compiling GHDL $GHDLVersion for Windows..." -ForegroundColor DarkCyan
		# Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
		
		Write-Host
		Write-Host "Running compile-ghdl.ps1 -All ..." -ForegroundColor DarkCyan
		Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
		$InvokeExpr = "$Script_Path " + ($Script_Parameters -join " ")
		Invoke-Expression $InvokeExpr
		if ($LastExitCode -ne 0)
		{	Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
			Write-Host "[ERROR]: While executing '$InvokeExpr'." -ForegroundColor Red
			Exit-Script -1
		}
		else
		{	Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
			Write-Host "Completed compile-ghdl.ps1 " -NoNewline
			Write-Host "[SUCCESSFUL]" -ForegroundColor Green
			Write-Host
		}
	}  # Compile_GHDL
	if ($Compile_Libraries)
	{	if ($Compile_GHDL)
		{	Write-Host    }
		
		$Script_Path =        $GHDLWindowsDir + "\compile-libraries.ps1"
		$Script_Parameters =  @()
		$Script_Parameters =  @(
			'-Compile',
			'-Hosted',
			'-Verbose:$EnableVerbose',
			'-Debug:$EnableDebug'
		)
		
		# Write-Host "Compiling GHDL's libraries ..." -ForegroundColor DarkCyan
		# Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
		
		$env:GHDL = "$GHDLBuildDir\ghdl.exe"
		Write-Host ("Setting env:GHDL to '" + $env:GHDL + "'")

		Write-Host
		Write-Host "Running compile-libraries.ps1 -Compile ..." -ForegroundColor DarkCyan
		Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
		$InvokeExpr = "$Script_Path " + ($Script_Parameters -join " ")
		Invoke-Expression $InvokeExpr
		if ($LastExitCode -ne 0)
		{	Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
			Write-Host "[ERROR]: While executing '$InvokeExpr'." -ForegroundColor Red
			Exit-Script -1
		}
		else
		{	Write-Host "--------------------------------------------------------------------------------" -ForegroundColor DarkCyan
			Write-Host "Completed compile-libraries.ps1 " -NoNewline
			Write-Host "[SUCCESSFUL]" -ForegroundColor Green
			Write-Host
		}
	}  # Compile_GHDL
	
	# ============================================================================
	# Package tasks
	# ============================================================================
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
			Copy-Item "$GHDLRootDir\src\grt\vhpi_user.h"  "$GHDLZipPackageDir\include"      -ErrorAction SilentlyContinue
			# pre-compile scripts
			Copy-Item $GHDLVendorLibraryDir -Recurse      "$GHDLZipPackageDir\lib\vendors"  -ErrorAction SilentlyContinue
			# pre-compiled libraries
			Copy-Item $GHDLCompiledLibraryDir  -Recurse   "$GHDLZipPackageDir"              -ErrorAction SilentlyContinue

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
			Copy-Item "$GHDLRootDir\src\grt\vhpi_user.h"  "$InstallPath\include"      -Verbose:$EnableVerbose -ErrorAction SilentlyContinue
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
		Copy-Item "$GHDLRootDir\src\grt\vhpi_user.h"  "$InstallPath\include"      -Verbose:$EnableVerbose -ErrorAction SilentlyContinue
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
	
}  # Clean
	
Exit-Script
