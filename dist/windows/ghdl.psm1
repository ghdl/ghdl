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
#	Copyright (C) 2017-2018 Patrick Lehmann - Boetzingen, Germany
#	Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
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
	[Parameter(Mandatory=$true)][string]$GHDLBinaryPath,
	[Parameter(Mandatory=$true)][string]$WorkingDir
)

# $Module_VendorToolName =	$VendorToolName
$Module_WorkingDir =			$WorkingDir


function Exit-GHDLModule
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
	Remove-Module ghdl -Verbose:$false
	
	if ($ExitCode -eq 0)
	{	exit 0	}
	else
	{	Write-Host "[DEBUG]: HARD EXIT" -ForegroundColor Red
		exit $ExitCode
	}
}

function Get-GHDLBinary
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER GHDL
		Path to GHDL's binary directory.
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
	{	throw "Use adv. options '-GHDL' to set the GHDL executable."  }
	
	return $GHDLBinary
}

$GHDLBinaryPath = Get-GHDLBinary $GHDLBinaryPath

function Analyze-File
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER DestinationDirectory
		Undocumented
		.PARAMETER Library
		Undocumented
		.PARAMETER SourceFile
		Undocumented
		.PARAMETER VHDLVersion
		Undocumented
		.PARAMETER GHDLOptions
		Undocumented
		.PARAMETER SuppressWarnings
		Undocumented
		.PARAMETER Indentation
		Undocumented
		.PARAMETER Quiet
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[Parameter(Mandatory=$true)][string]$DestinationDirectory,
		[Parameter(Mandatory=$true)][string]$Library,
		[Parameter(Mandatory=$true)][string]$SourceFile,
		[Parameter(Mandatory=$false)][string]$VHDLVersion = "08",
		[Parameter(Mandatory=$false)][string[]]$GHDLOptions = @(),
		[Parameter(Mandatory=$false)][bool]$SuppressWarnings = $false,
		[Parameter(Mandatory=$false)][string]$Indentation = "",
		[Parameter(Mandatory=$false)][switch]$Quiet = $false
	)

	$EnableDebug =		-not $Quiet -and (                  $PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug  -or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])
	$Indent =         if ($EnableDebug) { "$Indentation    " } else { if ($EnableVerbose) { "$Indentation  " } else { $Indentation }}
	
	$InvokeExpr = "& '$GHDLBinaryPath' -a " + ($GHDLOptions -join " ") + " `"--workdir=$DestinationDirectory`" --work=$Library --std=$VHDLVersion " + $SourceFile + " 2>&1"
	$EnableDebug -and (Write-Host "${Indentation}Analyzing $SourceFile" -ForegroundColor Gray     ) | Out-Null
	$EnableDebug -and (Write-Host "${Indentation}  $InvokeExpr"         -ForegroundColor DarkGray ) | Out-Null
	$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings "$Indent"
	if (($LastExitCode -ne 0) -and $HaltOnError)
	{	throw "Error while analyzing '$SourceFile'."  }
}

function Analyze-Library
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		

		.PARAMETER DestinationDirectory
		Undocumented
		.PARAMETER Library
		Undocumented
		.PARAMETER SourceFiles
		Undocumented
		.PARAMETER VHDLVersion
		Undocumented
		.PARAMETER GHDLOptions
		Undocumented
		.PARAMETER SuppressWarnings
		Undocumented
		.PARAMETER HaltOnError
		Undocumented
		.PARAMETER Indentation
		Undocumented
		.PARAMETER Quiet
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[Parameter(Mandatory=$true)][string]$DestinationDirectory,
		[Parameter(Mandatory=$true)][string]$Library,
		[Parameter(Mandatory=$true)][string[]]$SourceFiles,
		[Parameter(Mandatory=$false)][string]$VHDLVersion = "08",
		[Parameter(Mandatory=$false)][string[]]$GHDLOptions = @(),
		[Parameter(Mandatory=$false)][string]$LibraryDirectory = "",
		[Parameter(Mandatory=$false)][bool]$SuppressWarnings = $false,
		[Parameter(Mandatory=$false)][bool]$HaltOnError = $true,
		[Parameter(Mandatory=$false)][string]$Indentation = "",
		[Parameter(Mandatory=$false)][switch]$Quiet = $false
	)

	$EnableDebug =		-not $Quiet -and (                  $PSCmdlet.MyInvocation.BoundParameters["Debug"])
	$EnableVerbose =	-not $Quiet -and ($EnableDebug  -or $PSCmdlet.MyInvocation.BoundParameters["Verbose"])
	$Indent =         if ($Debug) { "$Indentation  " } else { $Indentation }
	
	Write-Host "${Indent}Compiling library '$Library' ..." -ForegroundColor Yellow
	
	if ($LibraryDirectory -eq "")
	{	$EnableVerbose -and (Write-Host "${Indent}  Creating library $Library in '$Library' ..."  -ForegroundColor Gray	      ) | Out-Null
		$LibraryDirectory = "$DestinationDirectory\$Library\v$VHDLVersion"
	}
	else
	{	$EnableVerbose -and (Write-Host "${Indent}  Creating library $Library in '$LibraryDirectory'..." -ForegroundColor Gray) | Out-Null
		$LibraryDirectory = "$DestinationDirectory\$LibraryDirectory\v$VHDLVersion"
	}
	
	# FIXME: test if directory exists
	if (Test-Path -Path $LibraryDirectory)
	{ $EnableVerbose -and (Write-Host "${Indent}  [INFO] Library directory '$LibraryDirectory' already exists." -ForegroundColor Yellow ) | Out-Null }
	else
	{	$EnableDebug -and   (Write-Host "${Indent}    mkdir $LibraryDirectory"	-ForegroundColor DarkGray	) | Out-Null
		mkdir $LibraryDirectory | Out-Null
	}

	$ErrorCount = 0
	foreach ($File in $SourceFiles)
	{	try
		{	Analyze-File $LibraryDirectory $Library $File $VHDLVersion $GHDLOptions -SuppressWarnings:$SuppressWarnings -Indentation:"$Indentation  " -Verbose:$EnableVerbose -Debug:$EnableDebug -Quiet:$Quiet  }
		catch
		{	$ErrorCount += 1
			if ($HaltOnError)
			{	throw $_ }
		}
	}
	
	if ($ErrorCount -gt 0)
	{	throw "Detected $ErrorCount errors."  }
	
	cd $DestinationDirectory
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
		{	Write-Host "Empty pipeline!" -ForegroundColor Red	}
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
		{	Write-Host "Unsupported object in pipeline stream" -ForegroundColor Red }
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

function Write-HostExtended
{	<#
		.SYNOPSIS
		Indentation and coloring.
		
		.DESCRIPTION
		This CmdLet indents and colors output lines.
		
		.PARAMETER InputObject
		A object stream is required as an input.
		.PARAMETER Indentation
		Indentation string.
		.PARAMETER Color
		Forground color.
	#>
	[CmdletBinding()]
	param(
		[Parameter(ValueFromPipeline=$true)]
		$InputObject,
		
		[Parameter(Position=1)]
		[string]$Indentation = "",
		
		[Parameter(Position=2)]
		[System.ConsoleColor]$Color = (Get-Host).ui.rawui.ForegroundColor
	)

	begin
	{ }
	
	process
	{	if ($InputObject -is [String])
		{	Write-Host "${Indentation}$InputObject" -ForegroundColor $Color         }
		else
		{	Write-Host "Unsupported object in pipeline stream" -ForegroundColor Red }
	}

	end
	{ }
}

Export-ModuleMember -Function 'Exit-GHDLModule'

Export-ModuleMember -Function 'Get-GHDLBinary'

Export-ModuleMember -Function 'Analyze-File'
Export-ModuleMember -Function 'Analyze-Library'

Export-ModuleMember -Function 'Restore-NativeCommandStream'
Export-ModuleMember -Function 'Write-ColoredGHDLLine'
Export-ModuleMember -Function 'Write-HostExtended'
