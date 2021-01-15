# EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#  Authors:
#    Patrick Lehmann
# 
#  PowerShell Module:	The module provides common CmdLets for ...
# 
# Description:
# ------------------------------------
#  This PowerShell module provides CommandLets (CmdLets) to ...
#
# ==============================================================================
#  Copyright (C) 2016-2017 Patrick Lehmann
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

[CmdletBinding()]
param(
	[Parameter(Mandatory=$true)][string]$WorkingDir,
	[Parameter(Mandatory=$true)][Switch]$Hosted
)

$Module_WorkingDir =	$WorkingDir
$Module_Hosted =			$Hosted

function Exit-CompileScript
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
	cd $Module_WorkingDir
	# unload modules
	if (-not $Module_Hosted)
	{	Remove-Module shared  -Verbose:$false -Debug:$false
		Remove-Module targets -Verbose:$false -Debug:$false
	}
	exit $ExitCode
}

function New-LibraryDirectory
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER Directory
		Undocumented
	#>
	[CmdletBinding()]
	param(
		[Parameter(Mandatory=$true)][string]$Directory	#,
		# [Parameter(Mandatory=$true)][bool]$EnableVerbose
	)
	$EnableVerbose = $false
	if (Test-Path -Path $Directory)
	{	$EnableVerbose -and (Write-Host "    Directory '$Directory' already exists."	) | Out-Null	}
	else
	{	Write-Host "    Creating directory '$Directory'."
		New-Item -ItemType directory -Path $Directory -ErrorAction SilentlyContinue | Out-Null
	}
}

function Format-VHDLSourceFile
{	<#
		.SYNOPSIS
		Undocumented
		
		.DESCRIPTION
		Undocumented
		
		.PARAMETER Version
		Undocumented
		.PARAMETER InputObject
		A object stream is required as an input.
	#>
	[CmdletBinding()]
	param(
		[Parameter(Mandatory=$true)][string]$Version,
		[Parameter(ValueFromPipeline=$true)]$InputObject
	)
	
	begin
	{	$State = 1
		$Version = switch ($Version)
								{	"87"	{	87	}
									"93"	{	93	}
									"02"	{	2		}
									"08"	{	8		}
								}
	}
	
	process
	{	if ($InputObject -is [String])
		{	$Line = $InputObject.ToString()
			if ($Line.StartsWith("--START-V"))
			{	$State = switch ($Line.Substring(9, 2))
									{	"87"	{	87	}
										"93"	{	93	}
										"02"	{	2		}
										"08"	{	8		}
									}
			}
			elseif ($Line.StartsWith("--START-!V"))
			{	if ($Line.Substring(10, 2) -eq $Version)
				{	$State = 2	}
			}
			elseif ($Line.StartsWith("--END-V") -or $Line.StartsWith("--END-!V"))
			{	$State = 1		}
			else
			{	if ($State -eq 1)
				{	if ($Line.EndsWith("--V$Version"))
					{	Write-Output $Line		}
					elseif (-not (($Line -like "*--V??") -or ($Line.EndsWith("--!V$Version"))))
					{	Write-Output $Line		}
				}
				elseif ($State -eq $Version)
				{	Write-Output $Line			}
			}
		}
		else
		{	Write-Host "Unknown pipeline object type." -ForegroundColor Red		}
	}
	
	end
	{	
	}
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
		.PARAMETER Indent
		Indentation string.
	#>
	[CmdletBinding()]
	param(
		[Parameter(ValueFromPipeline=$true)]
		$InputObject
	)

	begin
	{	$LineRemainer = ""	}

	process
	{	if ($InputObject -is [System.Management.Automation.ErrorRecord])
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

function Write-ColoredGCCLine
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
		{	if ($InputObject -match ":\d+:\d+:\swarning:\s")
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
			else
			{	Write-Host "${Indent}$InputObject"		}
		}
		else
		{	Write-Host "Unsupported object in pipeline stream"		}
	}

	end
	{	$ErrorRecordFound		}
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
		This CmdLet colors GHDL output lines.
		
		.DESCRIPTION
		This CmdLet colors GHDL output lines. Warnings are prefixed with 'WARNING: '
		in yellow and errors are prefixed with 'ERROR: ' in red.
		
		.PARAMETER InputObject
		A object stream is required as an input.
		.PARAMETER Indent
		Indentation string.
	#>
	[CmdletBinding()]
	param(
		[Parameter(ValueFromPipeline=$true)]
		$InputObject,
		
		[Parameter(Position=1)]
		[string]$Indent = ""
	)

	begin
	{		}
	
	process
	{	if ($InputObject -is [String])
		{	Write-Host "${Indent}$InputObject"									}
		else
		{	Write-Host "Unsupported object in pipeline stream"	}
	}

	end
	{		}
}

function Test-GitRepository
{	<#
		.SYNOPSIS
		Returns true, if the current working directy is under git control.
	#>
	
	git rev-parse 2>&1 | Out-Null
	return $LastExitCode -eq 0
}

# export functions
Export-ModuleMember -Function 'Exit-CompileScript'

Export-ModuleMember -Function 'New-LibraryDirectory'
Export-ModuleMember -Function 'Format-VHDLSourceFile'

Export-ModuleMember -Function 'Restore-NativeCommandStream'
Export-ModuleMember -Function 'Write-ColoredGCCLine'
Export-ModuleMember -Function 'Write-ColoredGHDLLine'
Export-ModuleMember -Function 'Write-HostExtended'

Export-ModuleMember -Function 'Test-GitRepository'
