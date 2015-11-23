
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
		[switch]$SuppressWarnings = $false
	)

	begin
	{	$ErrorRecordFound = $false	}
	
	process
	{	if (-not $InputObject)
		{	Write-Host "Empty pipeline!"	}
		elseif ($InputObject -is [String])
		{	if ($InputObject.Contains("warning"))
			{	if (-not $SuppressWarnings)
				{	Write-Host "WARNING: "	-NoNewline -ForegroundColor Yellow
					Write-Host $InputObject
				}
			}
			else
			{	$ErrorRecordFound	= $true
				Write-Host "ERROR: "		-NoNewline -ForegroundColor Red
				Write-Host $InputObject
			}
		}
		else
		{	Write-Host "Unsupported object in pipeline stream"		}
	}

	end
	{	$ErrorRecordFound		}
}

function Collect-NativeCommandStream
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
	{		}
}
