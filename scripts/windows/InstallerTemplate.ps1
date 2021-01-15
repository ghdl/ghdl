# EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#  Authors:
#    Patrick Lehmann
# 
#  PowerShell Script:	Self-extracting (ZIP) installer for GHDL for Windows
# 
# Description:
# ------------------------------------
#  This is a PowerShell script (executable) which:
#    - writes a ZIP file form an internal BLOB variable (base64 encoded)
#    - extract the ZIP file's content to a destination directory
#
# ==============================================================================
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
# GHDL for Windows - GHDL installer script
# Use 'install.ps1 -Help' to see the integrated help page
# 
# .EXAMPLE
# #
# # Normal flow
# PS> .\install.ps1 -Install
# 
# # Advanced flow
# PS> .\compile.ps1 -AddToPath Machine -Install "C:\Tools\GHDL"
#
[CmdletBinding()]
Param(
	# install all files into a directory (xcopy deployment)
	[switch]$Install = $false,
	[Parameter(Mandatory=$false, ValueFromRemainingArguments=$true)]
	[String]$InstallDir = "",
	# update files
	[switch]$Update,

	# register GHDL in PATH
	[Parameter(Mandatory=$false)]
	[ValidateSet("Machine", "User", "Session", "Remove", "Pass")]
	[String]$AddToPath = "",
	
	# display this help"
	[switch]$Help
)

# save parameters and current working directory
$Script_ScriptDir =		$PSScriptRoot
$Script_WorkingDir =	Get-Location
$GHDLRootDir =				Convert-Path (Resolve-Path ($PSScriptRoot + "\" + $RelPathToRoot))

# set default values
$EnableVerbose =			$PSCmdlet.MyInvocation.BoundParameters["Verbose"]
$EnableDebug =				$PSCmdlet.MyInvocation.BoundParameters["Debug"]
if ($EnableVerbose -eq $null)	{	$EnableVerbose =	$false	}
if ($EnableDebug	 -eq $null)	{	$EnableDebug =		$false	}
if ($EnableDebug	 -eq $true)	{	$EnableVerbose =	$true		}

# Display help if no command was selected
$Help = $Help -or (-not ($Install -or $Update))

# configure some variables: paths, executables, directory names, ...
$GHDLVersion =				"0.34-dev"
$GHDLBackend =				"mcode"
$DefaultInstallPath =	"C:\Program Files (x86)\GHDL"				# This is the default path for 32-bit applications (x86-32)

Write-Host "================================================================================" -ForegroundColor Magenta
Write-Host "GHDL for Windows - GHDL install script" -ForegroundColor Magenta
Write-Host "================================================================================" -ForegroundColor Magenta

function Exit-Script
{	[CmdletBinding()]
	param(
		[int]$ExitCode = 0
	)
	cd $Script_WorkingDir
	exit $ExitCode
}

if ($Help)
{	Get-Help $MYINVOCATION.MyCommand.Path -Detailed
	Exit-Script
}

$EnvPath_ContainerMapping = @{
	Machine =	[EnvironmentVariableTarget]::Machine
	User =		[EnvironmentVariableTarget]::User
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
	{	$containerType =	$EnvPath_ContainerMapping[$Container]
		$persistedPaths =	[Environment]::GetEnvironmentVariable("Path", $containerType) -split ";"
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
	{	$containerType =	$EnvPath_ContainerMapping[$Container]
		$persistedPaths =	[Environment]::GetEnvironmentVariable("Path", $containerType) -split ";"
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

# ============================================================================
# Base64 encoded zip file content
# ============================================================================

# DATASECTION

# ============================================================================


# ============================================================================
# Install tasks
# ============================================================================
if ($Install)
{	Write-Host "Installing GHDL $GHDLVersion for Windows..."
	if ($InstallDir -eq "")
	{	$InstallPath = $DefaultInstallPath
	}
	else
	{	$InstallPath = $InstallDir			}
	$InstallPath = $InstallPath.TrimEnd("\")
	
	if (Test-Path -Path $InstallPath)
	{	Write-Host "[ERROR]: Directory '$InstallPath' already exists." -ForegroundColor Red
		Exit-Script -1
	}
	Write-Host "  Install directory: $InstallPath"
	Write-Host "  Creating directory '$InstallPath' and sub-directories..."
	New-Item -ItemType Directory -Path "$InstallPath"						-ErrorAction SilentlyContinue	| Out-Null

	# writing ZIP file to disk
	$TempFilePath =	[System.IO.Path]::GetTempFileName().TrimEnd("tmp") + "zip"
	Write-Host "  Writing temporary ZIP file: $TempFilePath"
	$CompressedFileContentAsBytes =	[System.Convert]::FromBase64String($CompressedFileContentInBase64)
	[System.IO.File]::WriteAllBytes("$TempFilePath", $CompressedFileContentAsBytes)
	
	Write-Host "  Extracting ZIP file to: $InstallPath"
	Microsoft.PowerShell.Archive\Expand-Archive "$TempFilePath" -DestinationPath $InstallPath -Force

	Remove-Item $TempFilePath
	
	
	if ($AddToPath -eq "")
	{	while($true)
		{	Write-Host "  Install GHDL in PATH at machine level? [" -NoNewline -ForegroundColor DarkCyan
			Write-Host "M" -NoNewline -ForegroundColor Cyan
			Write-Host "achine/" -NoNewline -ForegroundColor DarkCyan
			Write-Host "u" -NoNewline -ForegroundColor Cyan
			Write-Host "ser/" -NoNewline -ForegroundColor DarkCyan
			Write-Host "s" -NoNewline -ForegroundColor Cyan
			Write-Host "ession]: " -NoNewline -ForegroundColor DarkCyan
			$InstallInPath = (Read-Host).ToLower()
			if ($InstallInPath -in "m","u","s")
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
}	# Install
elseif ($Update)
{	Write-Host "Updating GHDL $GHDLVersion for Windows..."
	if ($InstallDir -eq "")
	{	$InstallPath = $DefaultInstallPath
	}
	else
	{	$InstallPath = $InstallDir			}
	$InstallPath = $InstallPath.TrimEnd("\")
	
	Write-Host "  Install directory: $InstallPath"
	if (Test-Path -Path $InstallPath)
	{	Write-Host "  Cleaning up installation directory '$InstallPath'." -ForegroundColor Yellow
		Get-ChildItem -Path $InstallPath -Depth 0 | foreach { Remove-Item $_.FullName -Recurse -Force }
	}
	
	Write-Host "  Creating directory sub-directories in '$InstallPath' ..."
	
	# writing ZIP file to disk
	$TempFilePath =									[System.IO.Path]::GetTempFileName()
	Write-Host "  Writing temporary ZIP file: $TempFilePath"
	$CompressedFileContentAsBytes =	[System.Convert]::FromBase64String($CompressedFileContentInBase64)
	[System.IO.File]::WriteAllBytes("$TempFilePath", $CompressedFileContentAsBytes)
	
	Write-Host "  Extracting ZIP file to: $InstallPath"
	Expand-Archive "$TempFilePath" -OutputPath $InstallPath -ShowProgress

	Remove-Item $TempFilePath

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
}	# Update
	
Exit-Script
