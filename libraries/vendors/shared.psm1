# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	PowerShell Module:	The module provides common CmdLets for the library
#											pre-compilation process.
# 
# Description:
# ------------------------------------
#	This PowerShell module provides CommandLets (CmdLets) to handle the GHDL.exe
#	output streams (stdout and stderr).
#
# ==============================================================================
#	Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
#	Copyright (C) 2017 Patrick Lehmann - Freiburg, Germany
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

[CmdletBinding()]
param(
	[Parameter(Mandatory=$true)][string]$VendorToolName,
	[Parameter(Mandatory=$true)][string]$WorkingDir
)

$Module_VendorToolName =	$VendorToolName
$Module_WorkingDir =			$WorkingDir

function Exit-CompileScript
{		<#
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
	
	cd $Module_WorkingDir
	
	# unload modules
	Remove-Module config -Verbose:$false
	Remove-Module shared -Verbose:$false
	
	if ($ExitCode -eq 0)
	{	exit 0	}
	else
	{	Write-Host "[DEBUG]: HARD EXIT" -ForegroundColor Red
		exit $ExitCode
	}
}

function Get-SourceDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER Source
		Undocumented
		.PARAMETER EnvSource
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[string]$Source,
		[string]$EnvSource
	)

	$VendorToolInstallationDirectory =  Get-VendorToolInstallationDirectory
	$VendorToolSourceDirectory =        Get-VendorToolSourceDirectory
	
	if ($Source -ne "")
	{	$SourceDirectory = $Source.TrimEnd("\")		}
	elseif ($EnvSource -ne "")
	{	$SourceDirectory = $EnvSource							}
	elseif ($VendorToolInstallationDirectory -ne "")
	{	$SourceDirectory = $VendorToolInstallationDirectory + "\" + $VendorToolSourceDirectory	}
	else
	{	Write-Host "[ERROR]: $Module_VendorToolName is not configured in '$ScriptDir\config.psm1'." -ForegroundColor Red
		Write-Host "  Use adv. options '-Source' and '-Output' or configure 'config.psm1'." -ForegroundColor Red
		Exit-CompileScript -1
	}
	
	if (-not (Test-Path $SourceDirectory -PathType Container))
	{	Write-Host "[ERROR]: Path '$SourceDirectory' does not exist." -ForegroundColor Red
		Exit-CompileScript -1
	}
	
	return Convert-Path (Resolve-Path $SourceDirectory)
}

function Get-DestinationDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER Output
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[string]$Output
	)
	if ($Output -ne "")
	{	$DestinationDirectory = $Output.TrimEnd("\")								}
	else
	{	$DestinationDirectory = Get-VendorToolDestinationDirectory	}
	
	if ($DestinationDirectory -eq "")
	{	Write-Host "[ERROR]: $Module_VendorToolName is not configured in '$ScriptDir\config.psm1'." -ForegroundColor Red
		Write-Host "  Use adv. options '-Source' and '-Output' or configure 'config.psm1'." -ForegroundColor Red
		Exit-CompileScript -1
	}
	
	if (-not [System.IO.Path]::IsPathRooted($DestinationDirectory))
	{	$DestinationDirectory = "$Module_WorkingDir\$DestinationDirectory"		}
	
	return $DestinationDirectory
}

function Get-GHDLBinary
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER GHDL
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[string]$GHDL
	)

	if ($GHDL -ne "")
	{	$GHDLBinary = $GHDL.TrimEnd("\")			+ "\ghdl.exe"	}
	elseif (Test-Path env:GHDL)
	{	$GHDLBinary = $env:GHDL.TrimEnd("\")	+ "\ghdl.exe"	}
	else
	{	$GHDLBinary = "ghdl.exe"														}
	
	if (-not (Test-Path $GHDLBinary -PathType Leaf))
	{	Write-Host "Use adv. options '-GHDL' to set the GHDL executable." -ForegroundColor Red
		Exit-CompileScript -1
	}
	
	return $GHDLBinary
}


function Get-VHDLVariables
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER VHDL93
		Undocumented
		.PARAMETER VHDL2008
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[bool]$VHDL93 =		$false,
		[bool]$VHDL2008 = $true
	)
	
	if ($VHDL93)
	{	$VHDLVersion =	"v93"
		$VHDLStandard =	"93c"
		$VHDLFlavor =		"synopsys"
	}
	elseif ($VHDL2008)
	{	$VHDLVersion =	"v08"
		$VHDLStandard = "08"
		$VHDLFlavor =		"synopsys"
	}
	else
	{	$VHDLVersion =	"v93"
		$VHDLStandard = "93c"
		$VHDLFlavor =		"synopsys"
	}
	return $VHDLVersion,$VHDLStandard,$VHDLFlavor
}

function New-DestinationDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER DestinationDirectory
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[Parameter(Mandatory=$true)][string]$DestinationDirectory
	)

	if (Test-Path $DestinationDirectory -PathType Container)
	{	Write-Host "Vendor directory '$DestinationDirectory' already exists." -ForegroundColor Yellow		}
	elseif (Test-Path $DestinationDirectory -PathType Leaf)
	{	Write-Host "[ERROR]: Vendor directory '$DestinationDirectory' already exists as a file." -ForegroundColor Red
		Exit-CompileScript -1
	}
	else
	{	Write-Host "Creating vendor directory: '$DestinationDirectory'." -ForegroundColor Yellow
		mkdir "$DestinationDirectory" -ErrorAction SilentlyContinue | Out-Null
	}
}

function Start-PackageCompilation
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER GHDLBinary
		Undocumented
		.PARAMETER GHDLOptions
		Undocumented
		.PARAMETER Library
		Undocumented
		.PARAMETER SourceFiles
		Undocumented
		.PARAMETER HaltOnError
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[Parameter(Mandatory=$true)][string]$GHDLBinary,
		[Parameter(Mandatory=$true)][string[]]$GHDLOptions,
		[Parameter(Mandatory=$true)][string]$DestinationDirectory,
		[Parameter(Mandatory=$true)][string]$Library,
		[Parameter(Mandatory=$true)][string]$VHDLVersion,
		[Parameter(Mandatory=$true)][string[]]$SourceFiles,
		[Parameter(Mandatory=$true)][bool]$SuppressWarnings,
		[Parameter(Mandatory=$true)][bool]$HaltOnError
	)
	# set default valuesvalues
	$EnableDebug =		[bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
	$EnableVerbose =	[bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug
	
	Write-Host "Compiling library '$Library' ..." -ForegroundColor Yellow
	$LibraryDirectory=	"$DestinationDirectory/$Library/$VHDLVersion"
	$EnableVerbose -and (Write-Host "  Creating library $Library ..."	-ForegroundColor Gray	) | Out-Null
	$EnableDebug -and   (Write-Host "    mkdir $LibraryDirectory"	-ForegroundColor DarkGray	) | Out-Null
	mkdir $LibraryDirectory -ErrorAction SilentlyContinue | Out-Null
	$EnableDebug -and		(Write-Host "    cd $LibraryDirectory"		-ForegroundColor DarkGray	) | Out-Null
	cd $LibraryDirectory
	$ErrorCount = 0
	foreach ($File in $SourceFiles)
	{	Write-Host "  Analyzing package file '$File'" -ForegroundColor DarkCyan
		$InvokeExpr = "& '$GHDLBinary' " + ($GHDLOptions -join " ") + " --work=$Library " + $File + " 2>&1"
		$EnableDebug -and		(Write-Host "    $InvokeExpr" -ForegroundColor DarkGray	) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "  "
		if ($LastExitCode -ne 0)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	break		}
		}
	}
	
	cd $DestinationDirectory
	# return $ErrorCount
}

function Start-PrimitiveCompilation
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER GHDLBinary
		Undocumented
		.PARAMETER GHDLOptions
		Undocumented
		.PARAMETER Library
		Undocumented
		.PARAMETER SourceFiles
		Undocumented
		.PARAMETER HaltOnError
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[Parameter(Mandatory=$true)][string]$GHDLBinary,
		[Parameter(Mandatory=$true)][string[]]$GHDLOptions,
		[Parameter(Mandatory=$true)][string]$DestinationDirectory,
		[Parameter(Mandatory=$true)][string]$Library,
		[Parameter(Mandatory=$true)][string]$VHDLVersion,
		[Parameter(Mandatory=$true)][string[]]$SourceFiles,
		[Parameter(Mandatory=$true)][bool]$SuppressWarnings,
		[Parameter(Mandatory=$true)][bool]$HaltOnError
	)
	# set default values
	$EnableDebug =		[bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
	$EnableVerbose =	[bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug
	
	Write-Host "Compiling library '$Library' ..." -ForegroundColor Yellow
	$LibraryDirectory="$DestinationDirectory/$Library/$VHDLVersion"
	$EnableVerbose -and (Write-Host "  Creating library $Library ..."	-ForegroundColor Gray	) | Out-Null
	$EnableDebug -and		(Write-Host "    mkdir $LibraryDirectory"	-ForegroundColor DarkGray	) | Out-Null
	mkdir $LibraryDirectory -ErrorAction SilentlyContinue | Out-Null
	$EnableDebug -and		(Write-Host "    cd $LibraryDirectory"		-ForegroundColor DarkGray	) | Out-Null
	cd $LibraryDirectory
	$ErrorCount = 0
	foreach ($File in $SourceFiles)
	{	Write-Host "  Analyzing primitive file '$File'" -ForegroundColor DarkCyan
		$InvokeExpr = "& '$GHDLBinary' " + ($GHDLOptions -join " ") + " --work=$Library " + $File + " 2>&1"
		$EnableDebug -and		(Write-Host "    $InvokeExpr" -ForegroundColor DarkGray	) | Out-Null
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "  "
		if ($LastExitCode -ne 0)
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	break		}
		}
	}
	
	cd $DestinationDirectory
	# return $ErrorCount
}


function Restore-NativeCommandStream
{	<#
		.SYNOPSIS
		This CmdLet gathers multiple ErrorRecord objects and reconstructs outputs
		as a single line.
		
		.DESCRIPTION
		This CmdLet collects multiple ErrorRecord objects and emits one String
		object per line.
		
		.PARAMETER InputObject
		A object stream is required as an input.
	#>
	[CmdletBinding()]
	param(
		[Parameter(ValueFromPipeline=$true)]
		$InputObject
	)

	begin
	{	$LineRemainer = ""	}

	process
	{	if (-not $InputObject)
		{	Write-Host "Empty pipeline!"	}
		elseif ($InputObject -is [System.Management.Automation.ErrorRecord])
		{	if ($InputObject.FullyQualifiedErrorId -eq "NativeCommandError")
			{	Write-Output $InputObject.ToString()		}
			elseif ($InputObject.FullyQualifiedErrorId -eq "NativeCommandErrorMessage")
			{	$NewLine = $LineRemainer + $InputObject.ToString()
				while (($NewLinePos = $NewLine.IndexOf("`n")) -ne -1)
				{	Write-Output $NewLine.Substring(0, $NewLinePos)
					$NewLine = $NewLine.Substring($NewLinePos + 1)
				}
				$LineRemainer = $NewLine
			}
		}
		elseif ($InputObject -is [String])
		{	Write-Output $InputObject		}
		else
		{	Write-Host "Unsupported object in pipeline stream"		}
	}

	end
	{	if ($LineRemainer -ne "")
		{	Write-Output $LineRemainer	}
	}
}

function Write-ColoredGHDLLine
{	<#
		.SYNOPSIS
		This CmdLet colors GHDL output lines.
		
		.DESCRIPTION
		This CmdLet colors GHDL output lines. Warnings are prefixed with 'WARNING: '
		in yellow and errors are prefixed with 'ERROR: ' in red.
		
		.PARAMETER InputObject
		A object stream is required as an input.
		.PARAMETER SuppressWarnings
		Skip warning messages. (Show errors only.)
		.PARAMETER Indent
		Indentation string.
	#>
	[CmdletBinding()]
	param(
		[Parameter(ValueFromPipeline=$true)]
		$InputObject,
		
		[Parameter(Position=1)]
		[switch]$SuppressWarnings = $false,
		[Parameter(Position=2)]
		[string]$Indent = ""
	)

	begin
	{	$ErrorRecordFound = $false	}
	
	process
	{	if ($InputObject -is [String])
		{	if ($InputObject -match ":\d+:\d+:warning:\s")
			{	if (-not $SuppressWarnings)
				{	Write-Host "${Indent}WARNING: "	-NoNewline -ForegroundColor Yellow
					Write-Host $InputObject
				}
			}
			elseif ($InputObject -match ":\d+:\d+:\s")
			{	$ErrorRecordFound	= $true
				Write-Host "${Indent}ERROR: "		-NoNewline -ForegroundColor Red
				Write-Host $InputObject
			}
			elseif ($InputObject -match ":error:\s")
			{	$ErrorRecordFound	= $true
				Write-Host "${Indent}ERROR: "		-NoNewline -ForegroundColor Red
				Write-Host $InputObject
			}
			else
			{	Write-Host "${Indent}$InputObject"		}
		}
		else
		{	Write-Host "Unsupported object in pipeline stream"		}
	}

	end
	{	$ErrorRecordFound		}
}

Export-ModuleMember -Function 'Exit-CompileScript'

Export-ModuleMember -Function 'Get-SourceDirectory'
Export-ModuleMember -Function 'Get-DestinationDirectory'
Export-ModuleMember -Function 'Get-GHDLBinary'

Export-ModuleMember -Function 'Get-VHDLVariables'

Export-ModuleMember -Function 'New-DestinationDirectory'
Export-ModuleMember -Function 'Start-PackageCompilation'
Export-ModuleMember -Function 'Start-PrimitiveCompilation'


Export-ModuleMember -Function 'Restore-NativeCommandStream'
Export-ModuleMember -Function 'Write-ColoredGHDLLine'
