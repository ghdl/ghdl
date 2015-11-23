
function Write-ColoredGHDLLine
{	<#
		.SYNOPSIS
		Describe the function here
		.DESCRIPTION
		Describe the function in more detail
		.PARAMETER Line
		The line to print on screen
	#>
	[CmdletBinding()]
	param([Parameter(ValueFromPipeline=$true)]$InputObject)

	begin
	{	$ErrorRecordFound = $false	}
	
	process
	{	if (-not $InputObject)
		{	Write-Host "Empty pipeline!"	}
		elseif ($InputObject -is [String])
		{	if ($InputObject.Contains("warning"))
			{	Write-Host "WARNING: "	-NoNewline -ForegroundColor Yellow	}
			else
			{	$ErrorRecordFound	= $true
				Write-Host "ERROR: "		-NoNewline -ForegroundColor Red
			}
			Write-Host $InputObject
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
		Describe the function here
		.DESCRIPTION
		Describe the function in more detail
		.PARAMETER InputObject
		A stream of objects.
	#>
	[CmdletBinding()]
	param([Parameter(ValueFromPipeline=$true)]$InputObject)

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
