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
#		- compiles GHDL and GHDLFilter
#		- analyses VHDL libraries
#		- installs GHDL into a directory (xcopy deploiment)
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
# Use 'winbuild.ps1 -Help' to see the integrated help page
# 
# .EXAMPLE
# #
# # Normal flow
# PS> .\winbuild.ps1 -Clean
# PS> .\winbuild.ps1 -Compile
# PS> .\winbuild.ps1 -Install "C:\Tools\GHDL"
# 
# # Create a Zip-file
# PS>.\winbuild.ps1 -CreatePackage -Zip 
# 
# # combine all commands in a single call
# PS>.\winbuild.ps1 -Clean -Compile -Install "C:\Tools\GHDL"
#
[CmdletBinding()]
Param(
	# clean up all files and directories
	[switch]$Clean,
		[switch]$Clean_GHDL,
		[switch]$Clean_Libraries,
		
	# compile GHDL
	[switch]$Compile,
		[switch]$Compile_GHDL,
		[switch]$Compile_Libraries,

	# create an installer package
	[switch]$CreatePackage,
		# creates a zip-file for xcopy deployment
		[switch]$Zip,
	
	# install all files into a directory (xcopy deployment)
	[switch]$Install,
		# Installation directory
		[string]$InstallPath,
		# update files
		[switch]$Update,
	
	# uninstall all files from a directory
	[switch]$Uninstall,
	
	# display this help"
	[switch]$Help
)

# configure script here
$RelPathToRoot =			"..\.."

# save parameters and current working directory
$Script_ScriptDir =		$PSScriptRoot
$Script_WorkingDir =	Get-Location
$GHDLRootDir =				Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $RelPathToRoot))

# set default values
$EnableVerbose =			$PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent
$EnableDebug =				$PSCmdlet.MyInvocation.BoundParameters["Debug"].IsPresent

# Write-Host ("--> " + $Verbose + " value: " +$PSCmdlet.MyInvocation.BoundParameters["Verbose"] + " IsPresent: " + $PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent)
# Write-Host ("--> " + $PSCommandPath + "  " + $PSBoundParameters + "  " + $PSCmdlet + "  " + $PSDefaultParameterValues)

# load modules from GHDL's 'libraries' directory
Import-Module $PSScriptRoot\windows\shared.psm1 -Verbose:$false -ArgumentList "$Script_WorkingDir"
# Import-Module $PSScriptRoot\windows\targets.psm1 -Verbose:$false

# Display help if no command was selected
$Help = $Help -or (-not (
					$All -or 
					$Clean -or $Clean_GHDL -or $Clean_Libraries -or $Clean_Package_Zip -or
					$Compile -or $Compile_GHDL -or $Compile_Libraries -or
					$CreatePackage
				))

Write-Host "================================================================================" -ForegroundColor Magenta
Write-Host "GHDL for Windows - GHDL compile and bundle script" -ForegroundColor Magenta
Write-Host "================================================================================" -ForegroundColor Magenta

if ($Help)
{	Get-Help $MYINVOCATION.InvocationName -Detailed
	Exit-CompileScript
}

if ($All)
{	$Clean =							$true
	$Compile =						$true
	$CreatePackage =			$true
}
if ($Clean)
{	$Clean_GHDL =					$true
	$Clean_Libraries =		$true
	$Clean_Package_Zip =	$true
}
if ($Compile)
{	$Compile_GHDL =				$true
	$Compile_Libraries =	$true
}


# configure some variables: paths, executables, directory names, ...
$WindowsDirName =					"dist\mcode\windows"
$BuildDirName =						"dist\mcode\build"
$CompiledLibraryDirName =	"dist\mcode\lib"
$ZipPackageDirName =			"dist\mcode\zip"
$ZipPackageFileName =			"dist\mcode\ghdl-install.zip"
$VendorLibraryDirName =		"libraries\vendors"

# construct directories
$GHDLWindowsDir =						$GHDLRootDir + "\" + $WindowsDirName
$GHDLBuildDir =							$GHDLRootDir + "\" + $BuildDirName
$GHDLCompiledLibraryDir =		$GHDLRootDir + "\" + $CompiledLibraryDirName
$GHDLZipPackageDir =				$GHDLRootDir + "\" + $ZipPackageDirName
$GHDLZipPackageFile =				$GHDLRootDir + "\" + $ZipPackageFileName
$GHDLVendorLibraryDirName =	$GHDLRootDir + "\" + $VendorLibraryDirName

function Exit-Script
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER ExitCode
		ExitCode of this script run
	#>
	[CmdletBinding()]
	param(
		[int]$ExitCode = 0
	)
	
	cd $Script_WorkingDir
	
	# unload modules
	Remove-Module shared
	
	exit $ExitCode
}

# Author:	Ed Wilson
# Source:	https://blogs.technet.com/b/heyscriptingguy/archive/2011/07/23/use-powershell-to-modify-your-environmental-path.aspx
function Add-Path
	(	[parameter(Mandatory=$True,ValueFromPipeline=$True,Position=0)]
		[string]$AddedFolder
	)
# function body
	{	# Get the current search path from the environment keys in the registry.
		$OldPath = (Get-ItemProperty -Path "Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment" -Name PATH).Path

		# See if a new folder has been supplied.
		if (!$AddedFolder)
			{	return "No Folder Supplied. $ENV:PATH Unchanged"	}

		# See if the new folder exists on the file system.
		if (!(Test-Path $AddedFolder))
			{	return "Folder Does not Exist, Cannot be added to $ENV:PATH"	}

		# See if the new Folder is already in the path.
		if ($ENV:Path | Select-String -SimpleMatch $AddedFolder)
			{	return "Folder already within $ENV:PATH"	}

		# Set the New Path
		$NewPath = $OldPath + ";" + $AddedFolder

		Set-ItemProperty -Path "Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment" -Name PATH -Value $NewPath
	}

# Author:	Ed Wilson
# Source:	https://blogs.technet.com/b/heyscriptingguy/archive/2011/07/23/use-powershell-to-modify-your-environmental-path.aspx
function Remove-Path
	(	[parameter(Mandatory=$True,ValueFromPipeline=$True,Position=0)]
		[string]$RemovedFolder
	)
# function body
	{	# Get the Current Search Path from the environment keys in the registry
		$OldPath = (Get-ItemProperty -Path "Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment" -Name PATH).Path

		# Find the value to remove, replace it with $NULL. If it’s not found, nothing will change.
		$NewPath = $OldPath -replace $RemovedFolder,$Null

		# Update the Environment Path
		Set-ItemProperty -Path "Registry::HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session Manager\Environment" -Name PATH -Value $NewPath
	}
	

# if ($Help)
	# {	Write-Host "Usage:"
		# Write-Host "  compile.ps1 [-Verbose] [-Debug] (-Help|-Compile|-Clean|-CreatePackage|-Install|-Uninstall)" -ForegroundColor Gray
		# Write-Host
		# Write-Host "Options:"
		# Write-Host "  -Verbose    enable detailed messages"
		# Write-Host "  -Debug      enable debug messages"
		# Write-Host
		# Write-Host "Commands:"
		# Write-Host "  -Help             display this help"
		# Write-Host "  -Compile          compile all library files"
		# Write-Host "  -Clean            clean up all files and directories"
		# Write-Host "  -CreatePackage    create an installer package"
		# Write-Host "  -Install          install all files into a directory (xcopy deployment)"
		# Write-Host "  -Uninstall        uninstall all files from a directory"
		# Write-Host
		# Write-Host "Options for -CreatePackage:"
		# Write-Host "  -Zip              creates a zip-file for xcopy deployment"
		# Write-Host
		# Write-Host "Options for -Install:"
		# Write-Host "  -InstallPath <dir>  directory into which GHDL will be installed"
		# Write-Host
		# Write-Host "Examples:"
		# Write-Host "  # Normal flow"
		# Write-Host "  PS>.\winbuild.ps1 -Clean" -ForegroundColor Gray
		# Write-Host "  PS>.\winbuild.ps1 -Compile" -ForegroundColor Gray
		# Write-Host "  PS>.\winbuild.ps1 -Install -InstallPath `"C:\Tools\GHDL`"" -ForegroundColor Gray
		# Write-Host
		# Write-Host "  # Create a zip-file"
		# Write-Host "  PS>.\winbuild.ps1 -CreatePackage -Zip" -ForegroundColor Gray
		# Write-Host
		# Write-Host "  # combine all commands in a single call"
		# Write-Host "  PS>.\winbuild.ps1 -Clean -Compile -Install -InstallPath `"C:\Tools\GHDL`"" -ForegroundColor Gray
		# Write-Host
	# }
# else
if ($Install)
{	Write-Host "Installing GHDL $GHDLVersion for Windows..."

	Write-Host "This command is not implemented." -ForegroundColor Red
	Exit-Script -1
}	# Install
elseif ($Update)
{	Write-Host "Updating GHDL $GHDLVersion for Windows..."

	Write-Host "This command is not implemented." -ForegroundColor Red
	Exit-Script -1
}	# Update
elseif ($Uninstall)
{	Write-Host "Uninstalling GHDL $GHDLVersion for Windows..."

	Write-Host "This command is not implemented." -ForegroundColor Red
	Exit-Script -1
}	# Uninstall
else
{	# ============================================================================
	# Clean tasks
	# ============================================================================
	if ($Clean)
	{	Write-Host "Removing all created files and directories..."		}
	
	if ($Clean_GHDL)
	{	$Script_Path = 				$GHDLWindowsDir + "\compile-ghdl.ps1"
		$Script_Parameters =	@(
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
	}	# Clean_GHDL
	if ($Clean_Libraries)
	{	$Script_Path = 				$GHDLWindowsDir + "\compile-libraries.ps1"
		$Script_Parameters =	@(
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
	}	# Clean_Libraries
	if ($Clean_Package_Zip)
	{	Write-Host "Removing installer packages and temporary directories..."
		if (Test-Path -Path $GHDLZipPackageDir)
		{	Write-Host "  rmdir $GHDLZipPackageDir"
			Remove-Item $GHDLZipPackageDir -Force -Recurse -ErrorAction SilentlyContinue
			if ($? -eq $false)
			{	Write-Host "[ERROR]: While deleting '$GHDLZipPackageDir'." -ForegroundColor Red
				Exit-Script -1
			}
		}
		
		Write-Host "  $GHDLZipPackageFile"
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
	}	# Clean_Package_Zip
	
	# ============================================================================
	# Compile tasks
	# ============================================================================
	if ($Compile_GHDL)
	{	Write-Host "Compiling GHDL $GHDLVersion for Windows..."
			
		$Script_Path = 				$GHDLWindowsDir + "\compile-ghdl.ps1"
		$Script_Parameters =	@()
		$Script_Parameters =	@(
			'-All',
			'-Verbose:$EnableVerbose',
			'-Debug:$EnableDebug'
		)
					
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
	}	# Compile_GHDL
	if ($Compile_Libraries)
	{	Write-Host "Compiling GHDL's libraries ..."
			
		$Script_Path = 				$GHDLWindowsDir + "\compile-libraries.ps1"
		$Script_Parameters =	@()
		$Script_Parameters =	@(
			'-All',
			'-Verbose:$EnableVerbose',
			'-Debug:$EnableDebug'
		)
		
		$env:GHDL = "$GHDLBuildDir\ghdl.exe"
		Write-Host "env:GHDL --" + $env:GHDL + "--"
		
		Write-Host "Running compile-libraries.ps1 -All ..." -ForegroundColor DarkCyan
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
	}	# Compile_GHDL
	
	
	
}	# Clean
	
	
# 		
# 	if ($CreatePackage)
# 		{	$Script_ExitCode = 0
# 			Write-Host "Creating an installation package for GHDL $GHDLVersion for Windows"
# 
# 			if ($Zip)
# 				{	if ((Get-Module -ListAvailable | Where {$_.Name -like "PSCX"}).Version -ge "3.1.0.0")
# 						{	Write-Host "Loading PowerShell Community Extensions (PSCX) " -NoNewline
# 							Import-Module Pscx
# 							Write-Host "[Done]" -ForegroundColor Green
# 						}
# 					else
# 						{	$Script_ExitCode = 1
# 							Write-Host "[FAILED]" -ForegroundColor RED	
# 						}
# 				}
# 			
# 			# create zip-file
# 			if (($Script_ExitCode -eq 0) -and $Zip)
# 				{	Write-Host "Output format: zip-file"
# 				
# 					Write-Host "  Removing old directory '$GHDLZipPackageDir'."
# 					Remove-Item $GHDLZipPackageDir -Force -Recurse -ErrorAction SilentlyContinue
# 				
# 					Write-Host "  Creating directory '$GHDLZipPackageDir'."
# 					[void](New-Item -ItemType directory -Path "$GHDLZipPackageDir"					-ErrorAction SilentlyContinue)
# 					[void](New-Item -ItemType directory -Path "$GHDLZipPackageDir\bin"			-ErrorAction SilentlyContinue)
# 					[void](New-Item -ItemType directory -Path "$GHDLZipPackageDir\scripts"	-ErrorAction SilentlyContinue)
# 				
# 					Copy-Item "$GHDLBuildDir\ghdl.exe"				"$GHDLZipPackageDir\bin\ghdl.exe"				-ErrorAction SilentlyContinue
# 					Copy-Item "$GHDLBuildDir\ghdlfilter.exe"	"$GHDLZipPackageDir\bin\ghdlfilter.exe"	-ErrorAction SilentlyContinue
# 
# 					Copy-Item $GHDLCompiledLibraryDir		-Recurse	"$GHDLZipPackageDir"					-ErrorAction SilentlyContinue
# 					Copy-Item $GHDLVendorLibraryDirName	-Recurse	"$GHDLZipPackageDir\scripts"	-ErrorAction SilentlyContinue
# 
# 					Write-Host "  Compressing files into '$GHDLZipPackageFile'"
# 					$file = Get-ChildItem $GHDLZipPackageDir -Recurse | Write-Zip -IncludeEmptyDirectories -EntryPathRoot $GHDLZipPackageDir -OutputPath $GHDLZipPackageFile
# 					Write-Host "  $([math]::round(($file.Length / 1MB), 3)) MiB written to disk"
# 				}
# 			else
# 				{	$Script_ExitCode = 1
# 					Write-Host "No package format selected." -ForegroundColor Red
# 					Write-Host "Possible formats:"
# 					Write-Host "  - zip-file (-Zip)"
# 					Write-Host
# 				}
# 			
# 			if ($Script_ExitCode -eq 0)
# 				{	Write-Host
# 					Write-Host "Create Package " -NoNewline
# 					Write-Host "[SUCCESSFUL]" -ForegroundColor Green
# 					Write-Host
# 				}
# 		}	# CreatePackage
# 		
# 	if ($Install)
# 		{	$Script_ExitCode = 0
# 			Write-Host "Installing GHDL $GHDLVersion for Windows"
# 
# 			if ($InstallPath -eq "")
# 				{	$Script_ExitCode = 1
# 					Write-Host "Missing argument -InstallPath" -ForegroundColor Red
# 				}
# 			else
# 				{	if (Test-Path -Path $InstallPath)
# 						{	if ($Update)
# 								{	Remove-Item	-Path "$InstallPath\*" -Recurse -Force	}
# 							else
# 								{	Write-Host "  Directory '$InstallPath' already exists." -ForegroundColor Red
# 									Write-Host
# 									$Script_ExitCode = 1
# 								}
# 						}
# 					elseif ($Update)
# 						{	Write-Host "  Directory '$InstallPath' does not exists." -ForegroundColor Red
# 							Write-Host
# 							$Script_ExitCode = 1
# 						}
# 				}
# 				
# 			if ($Script_ExitCode -eq 0)
# 				{	Write-Host "  Install directory: $InstallPath"
# 				
# 					Write-Host "  Creating directory '$InstallPath'."
# 					[void](New-Item -ItemType directory -Path "$InstallPath"					-ErrorAction SilentlyContinue)
# 					[void](New-Item -ItemType directory -Path "$InstallPath\bin"			-ErrorAction SilentlyContinue)
# 					[void](New-Item -ItemType directory -Path "$InstallPath\scripts"	-ErrorAction SilentlyContinue)
# 					
# 					Copy-Item "$GHDLBuildDir\ghdl.exe"				"$InstallPath\bin\ghdl.exe"				-ErrorAction SilentlyContinue
# 					Copy-Item "$GHDLBuildDir\ghdlfilter.exe"	"$InstallPath\bin\ghdlfilter.exe"	-ErrorAction SilentlyContinue
# 
# 					Copy-Item $GHDLCompiledLibraryDir					-Recurse	"$InstallPath"					-ErrorAction SilentlyContinue
# 					Copy-Item "$GHDLVendorLibraryDirName\*.*"	-Recurse	"$InstallPath\scripts"	-ErrorAction SilentlyContinue
# 				}
# 			
# 			if ($Script_ExitCode -eq 0)
# 				{	Write-Host "  Registering installation directory in system PATH" -NoNewline
# 					Write-Host "  [DISABLED]" -ForegroundColor Red
# 					#Add-Path "$InstallPath\bin"
# 				}
# 			
# 			if ($Script_ExitCode -eq 0)
# 				{	Write-Host
# 					Write-Host "Install " -NoNewline
# 					Write-Host "[SUCCESSFUL]" -ForegroundColor Green
# 					Write-Host
# 				}
# 		}	# Install
# 
# 	if ($Script_ExitCode -eq -1)
# 		{	Write-Host "ERROR: missing argument(s)" -ForegroundColor Red
# 			Write-Host
# 			Write-Host "Usage:"
# 			Write-Host "  winbuild.ps1 [-Verbose] [-Debug] (-Help|-Compile|-Clean|-CreatePackage|-Install|-Uninstall)" -ForegroundColor Gray
# 			Write-Host
# 		}	# Unknown
# }

# restore working directory if changed
Set-Location $Script_WorkingDir

# return exit status
exit $Script_ExitCode
