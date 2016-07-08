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
<#
	.SYNOPSIS 
		GHDL for Windows - GHDL compile script
		Use 'winbuild.ps1 -Help' to see the integrated help page
	
	.EXAMPLE
		#
		# Normal flow
		PS> .\winbuild.ps1 -Clean
		PS> .\winbuild.ps1 -Compile
		PS> .\winbuild.ps1 -Install -InstallPath "C:\Tools\GHDL"

		# Create a zip-file
		PS>.\winbuild.ps1 -CreatePackage -Zip 

		# combine all commands in a single call
		PS>.\winbuild.ps1 -Clean -Compile -Install -InstallPath "C:\Tools\GHDL"
#>

# define script parameters
[CmdletBinding()]
Param(
	# compile GHDL
	[switch]$Compile,
	
	# clean up all files and directories
	[switch]$Clean,

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
$Script_RelPathToRoot =	"..\.."

# save parameters and current working directory
$Script_Parameters =		$args
$Script_ScriptDir =			$PSScriptRoot
$Script_WorkingDir =		Get-Location
$GHDLRootDir_AbsPath =	Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $Script_RelPathToRoot))

# configure some variables: paths, executables, directory names, ...
$WindowsDirName =					"dist\mcode\windows"
$BuildDirName =						"dist\mcode\build"
$CompiledLibraryDirName =	"dist\mcode\lib"
$ZipPackageDirName =			"dist\mcode\zip"
$ZipPackageFileName =			"dist\mcode\ghdl-install.zip"
$VendorLibraryDirName =		"libraries\vendors"

# construct directories
$GHDLWindowsDir =						$GHDLRootDir_AbsPath + "\" + $WindowsDirName
$GHDLBuildDir =							$GHDLRootDir_AbsPath + "\" + $BuildDirName
$GHDLCompiledLibraryDir =		$GHDLRootDir_AbsPath + "\" + $CompiledLibraryDirName
$GHDLZipPackageDir =				$GHDLRootDir_AbsPath + "\" + $ZipPackageDirName
$GHDLZipPackageFile =				$GHDLRootDir_AbsPath + "\" + $ZipPackageFileName
$GHDLVendorLibraryDirName =	$GHDLRootDir_AbsPath + "\" + $VendorLibraryDirName

# set default values
$Script_ExitCode = 			0
if ($PSCmdlet.MyInvocation.BoundParameters["Debug"].IsPresent) 		{	$Script_EnableDebug =		$true	}
if ($PSCmdlet.MyInvocation.BoundParameters["Verbose"].IsPresent)	{	$Script_EnableVerbose =	$true	}

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
	

if ($Help)
	{	Write-Host "Usage:"
		Write-Host "  compile.ps1 [-Verbose] [-Debug] (-Help|-Compile|-Clean|-CreatePackage|-Install|-Uninstall)" -ForegroundColor Gray
		Write-Host
		Write-Host "Options:"
		Write-Host "  -Verbose    enable detailed messages"
		Write-Host "  -Debug      enable debug messages"
		Write-Host
		Write-Host "Commands:"
		Write-Host "  -Help             display this help"
		Write-Host "  -Compile          compile all library files"
		Write-Host "  -Clean            clean up all files and directories"
		Write-Host "  -CreatePackage    create an installer package"
		Write-Host "  -Install          install all files into a directory (xcopy deployment)"
		Write-Host "  -Uninstall        uninstall all files from a directory"
		Write-Host
		Write-Host "Options for -CreatePackage:"
		Write-Host "  -Zip              creates a zip-file for xcopy deployment"
		Write-Host
		Write-Host "Options for -Install:"
		Write-Host "  -InstallPath <dir>  directory into which GHDL will be installed"
		Write-Host
		Write-Host "Examples:"
		Write-Host "  # Normal flow"
		Write-Host "  PS>.\winbuild.ps1 -Clean" -ForegroundColor Gray
		Write-Host "  PS>.\winbuild.ps1 -Compile" -ForegroundColor Gray
		Write-Host "  PS>.\winbuild.ps1 -Install -InstallPath `"C:\Tools\GHDL`"" -ForegroundColor Gray
		Write-Host
		Write-Host "  # Create a zip-file"
		Write-Host "  PS>.\winbuild.ps1 -CreatePackage -Zip" -ForegroundColor Gray
		Write-Host
		Write-Host "  # combine all commands in a single call"
		Write-Host "  PS>.\winbuild.ps1 -Clean -Compile -Install -InstallPath `"C:\Tools\GHDL`"" -ForegroundColor Gray
		Write-Host
	}
elseif ($Uninstall)
	{	Write-Host "Uninstalling GHDL $GHDLVersion for Windows"

		Write-Host "This command is not implemented" -ForegroundColor Red
		$Script_ExitCode = 1
	
		if ($Script_ExitCode -eq 0)
			{	Write-Host
				Write-Host "Uninstall " -NoNewline
				Write-Host "[SUCCESSFUL]" -ForegroundColor Green
				Write-Host
			}
	}	# Uninstall
else
	{ $Script_ExitCode = -1
		
		if ($Clean)
			{	$Script_ExitCode = 0
				Write-Host "Removing all created files and directories..."
		
				if ($Script_ExitCode -eq 0)
					{	$Script_Path = 				$GHDLWindowsDir + "\compile-ghdl.ps1"
						$Script_Parameters =	@('-Clean')
						#$Script_Parameters +=	'-Clean'
						if ($Script_EnableVerbose -eq $true)	{	$Script_Parameters += '-Verbose'	}
						if ($Script_EnableDebug -eq $true)		{	$Script_Parameters += '-Debug'		}
						
						Write-Host "Running compile-ghdl.ps1 ..."
						Write-Host "--------------------------------------------------------------------------------"
						Invoke-Expression "$Script_Path $($Script_Parameters -join " ")"
						if ($LastExitCode -ne 0)
							{	$Script_ExitCode = 1
								Write-Host "--------------------------------------------------------------------------------"
								Write-Host "ERROR while executing 'compile-ghdl.ps1 $($Script_Paramters -join " ")'" -ForegroundColor Red
							}
						else
							{	Write-Host "--------------------------------------------------------------------------------"
								Write-Host "Completed " -NoNewline
								Write-Host "[SUCCESSFUL]" -ForegroundColor Green
								Write-Host
							}
					}
				
				if ($Script_ExitCode -eq 0)
					{	$Script_Path = 				$GHDLWindowsDir + "\compile-ghdl.ps1"
						$Script_Parameters =	@()
						$Script_Parameters +=	'-Clean'
						if ($Script_EnableVerbose -eq $true)	{	$Script_Parameters += '-Verbose'	}
						if ($Script_EnableDebug -eq $true)		{	$Script_Parameters += '-Debug'		}
						
						Write-Host "Running compile-libraries.ps1 ..."
						Write-Host "--------------------------------------------------------------------------------"
						Invoke-Expression "$Script_Path $($Script_Parameters -join " ")"
						if ($LastExitCode -ne 0)
							{	$Script_ExitCode = 1
								Write-Host "--------------------------------------------------------------------------------"
								Write-Host "ERROR while executing 'compile-libraries.ps1 $($Script_Paramters -join " ")'" -ForegroundColor Red
							}
						else
							{	Write-Host "--------------------------------------------------------------------------------"
								Write-Host "Completed " -NoNewline
								Write-Host "[SUCCESSFUL]" -ForegroundColor Green
								Write-Host
							}
					}
					
				if ($Script_ExitCode -eq 0)
					{	Write-Host "Removing installer packages and temporary directories..."
						Write-Host "  $GHDLZipPackageDir"
						Remove-Item $GHDLZipPackageDir -Force -Recurse -ErrorAction SilentlyContinue
						
						Write-Host "  $GHDLZipPackageFile"
						Remove-Item $GHDLZipPackageFile -Force -Recurse -ErrorAction SilentlyContinue
					}
					
				if ($Script_ExitCode -eq 0)
					{	Write-Host
						Write-Host "Clean " -NoNewline
						Write-Host "[SUCCESSFUL]" -ForegroundColor Green
						Write-Host
					}
			}	# Clean
		
		if ($Compile)
			{	$Script_ExitCode = 0
				Write-Host "Compiling GHDL $GHDLVersion for Windows"
				
				if ($Script_ExitCode -eq 0)
					{	$Script_Path = 				$GHDLWindowsDir + "\compile-ghdl.ps1"
						$Script_Parameters =	@()
						$Script_Parameters +=	'-All'
						if ($Script_EnableVerbose -eq $true)	{	$Script_Parameters += '-Verbose'	}
						if ($Script_EnableDebug -eq $true)		{	$Script_Parameters += '-Debug'		}
						
						Write-Host "Running compile-ghdl.ps1 ..."
						Write-Host "--------------------------------------------------------------------------------"
						Invoke-Expression "$Script_Path $($Script_Parameters -join " ")"
						if ($LastExitCode -ne 0)
							{	$Script_ExitCode = 1
								Write-Host "--------------------------------------------------------------------------------"
								Write-Host "ERROR while executing 'compile-ghdl.ps1 $($Script_Paramters -join " ")'" -ForegroundColor Red
							}
						else
							{	Write-Host "--------------------------------------------------------------------------------"
								Write-Host "Completed " -NoNewline
								Write-Host "[SUCCESSFUL]" -ForegroundColor Green
								Write-Host
							}
					}
				
				if ($Script_ExitCode -eq 0)
					{	$Script_Path = 				$GHDLWindowsDir + "\compile-libraries.ps1"
						$Script_Parameters =	@()
						$Script_Parameters +=	'-Compile'
						if ($Script_EnableVerbose -eq $true)	{	$Script_Parameters += '-Verbose'	}
						if ($Script_EnableDebug -eq $true)		{	$Script_Parameters += '-Debug'		}
						
						$env:GHDL = "$GHDLBuildDir\ghdl.exe"
						
						Write-Host "Running compile-libraries.ps1 ..."
						Write-Host "--------------------------------------------------------------------------------"
						Invoke-Expression "$Script_Path $($Script_Parameters -join " ")"
						if ($LastExitCode -ne 0)
							{	$Script_ExitCode = 1
								Write-Host "--------------------------------------------------------------------------------"
								Write-Host "ERROR while executing 'compile-libraries.ps1 $($Script_Paramters -join " ")'" -ForegroundColor Red
							}
						else
							{	Write-Host "--------------------------------------------------------------------------------"
								Write-Host "Completed " -NoNewline
								Write-Host "[SUCCESSFUL]" -ForegroundColor Green
								Write-Host
							}
					}
					
				if ($Script_ExitCode -eq 0)
					{	Write-Host
						Write-Host "Compile " -NoNewline
						Write-Host "[SUCCESSFUL]" -ForegroundColor Green
						Write-Host
					}
			}	# Compile
			
		if ($CreatePackage)
			{	$Script_ExitCode = 0
				Write-Host "Creating an installation package for GHDL $GHDLVersion for Windows"

				if ($Zip)
					{	if ((Get-Module -ListAvailable | Where {$_.Name -like "PSCX"}).Version -ge "3.1.0.0")
							{	Write-Host "Loading PowerShell Community Extensions (PSCX) " -NoNewline
								Import-Module Pscx
								Write-Host "[Done]" -ForegroundColor Green
							}
						else
							{	$Script_ExitCode = 1
								Write-Host "[FAILED]" -ForegroundColor RED	
							}
					}
				
				# create zip-file
				if (($Script_ExitCode -eq 0) -and $Zip)
					{	Write-Host "Output format: zip-file"
					
						Write-Host "  Removing old directory '$GHDLZipPackageDir'."
						Remove-Item $GHDLZipPackageDir -Force -Recurse -ErrorAction SilentlyContinue
					
						Write-Host "  Creating directory '$GHDLZipPackageDir'."
						[void](New-Item -ItemType directory -Path "$GHDLZipPackageDir"					-ErrorAction SilentlyContinue)
						[void](New-Item -ItemType directory -Path "$GHDLZipPackageDir\bin"			-ErrorAction SilentlyContinue)
						[void](New-Item -ItemType directory -Path "$GHDLZipPackageDir\scripts"	-ErrorAction SilentlyContinue)
					
						Copy-Item "$GHDLBuildDir\ghdl.exe"				"$GHDLZipPackageDir\bin\ghdl.exe"				-ErrorAction SilentlyContinue
						Copy-Item "$GHDLBuildDir\ghdlfilter.exe"	"$GHDLZipPackageDir\bin\ghdlfilter.exe"	-ErrorAction SilentlyContinue

						Copy-Item $GHDLCompiledLibraryDir		-Recurse	"$GHDLZipPackageDir"					-ErrorAction SilentlyContinue
						Copy-Item $GHDLVendorLibraryDirName	-Recurse	"$GHDLZipPackageDir\scripts"	-ErrorAction SilentlyContinue

						Write-Host "  Compressing files into '$GHDLZipPackageFile'"
						$file = Get-ChildItem $GHDLZipPackageDir -Recurse | Write-Zip -IncludeEmptyDirectories -EntryPathRoot $GHDLZipPackageDir -OutputPath $GHDLZipPackageFile
						Write-Host "  $([math]::round(($file.Length / 1MB), 3)) MiB written to disk"
					}
				else
					{	$Script_ExitCode = 1
						Write-Host "No package format selected." -ForegroundColor Red
						Write-Host "Possible formats:"
						Write-Host "  - zip-file (-Zip)"
						Write-Host
					}
				
				if ($Script_ExitCode -eq 0)
					{	Write-Host
						Write-Host "Create Package " -NoNewline
						Write-Host "[SUCCESSFUL]" -ForegroundColor Green
						Write-Host
					}
			}	# CreatePackage
			
		if ($Install)
			{	$Script_ExitCode = 0
				Write-Host "Installing GHDL $GHDLVersion for Windows"

				if ($InstallPath -eq "")
					{	$Script_ExitCode = 1
						Write-Host "Missing argument -InstallPath" -ForegroundColor Red
					}
				else
					{	if (Test-Path -Path $InstallPath)
							{	if ($Update)
									{	Remove-Item	-Path "$InstallPath\*" -Recurse -Force	}
								else
									{	Write-Host "  Directory '$InstallPath' already exists." -ForegroundColor Red
										Write-Host
										$Script_ExitCode = 1
									}
							}
						elseif ($Update)
							{	Write-Host "  Directory '$InstallPath' does not exists." -ForegroundColor Red
								Write-Host
								$Script_ExitCode = 1
							}
					}
					
				if ($Script_ExitCode -eq 0)
					{	Write-Host "  Install directory: $InstallPath"
					
						Write-Host "  Creating directory '$InstallPath'."
						[void](New-Item -ItemType directory -Path "$InstallPath"					-ErrorAction SilentlyContinue)
						[void](New-Item -ItemType directory -Path "$InstallPath\bin"			-ErrorAction SilentlyContinue)
						[void](New-Item -ItemType directory -Path "$InstallPath\scripts"	-ErrorAction SilentlyContinue)
						
						Copy-Item "$GHDLBuildDir\ghdl.exe"				"$InstallPath\bin\ghdl.exe"				-ErrorAction SilentlyContinue
						Copy-Item "$GHDLBuildDir\ghdlfilter.exe"	"$InstallPath\bin\ghdlfilter.exe"	-ErrorAction SilentlyContinue

						Copy-Item $GHDLCompiledLibraryDir					-Recurse	"$InstallPath"					-ErrorAction SilentlyContinue
						Copy-Item "$GHDLVendorLibraryDirName\*.*"	-Recurse	"$InstallPath\scripts"	-ErrorAction SilentlyContinue
					}
				
				if ($Script_ExitCode -eq 0)
					{	Write-Host "  Registering installation directory in system PATH" -NoNewline
						Write-Host "  [DISABLED]" -ForegroundColor Red
						#Add-Path "$InstallPath\bin"
					}
				
				if ($Script_ExitCode -eq 0)
					{	Write-Host
						Write-Host "Install " -NoNewline
						Write-Host "[SUCCESSFUL]" -ForegroundColor Green
						Write-Host
					}
			}	# Install

		if ($Script_ExitCode -eq -1)
			{	Write-Host "ERROR: missing argument(s)" -ForegroundColor Red
				Write-Host
				Write-Host "Usage:"
				Write-Host "  winbuild.ps1 [-Verbose] [-Debug] (-Help|-Compile|-Clean|-CreatePackage|-Install|-Uninstall)" -ForegroundColor Gray
				Write-Host
			}	# Unknown
	}

# restore working directory if changed
Set-Location $Script_WorkingDir

# return exit status
exit $Script_ExitCode
