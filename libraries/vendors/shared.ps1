
function Format-NativeCommandStreams
{	param([Parameter(ValueFromPipeline=$true)]$InputObject)

	begin
	{	$ErrorRecordFound = $false	}

	process
	{	if (-not $InputObject)
		{	Write-Host "Empty"	}
		elseif ($InputObject -is [System.Management.Automation.ErrorRecord])
		{	$ErrorRecordFound	= $true
			$text = $InputObject.ToString()
			Write-Host $text -ForegroundColor	Gray
			
			$stdErr = $InputObject.TargetObject
			if ($stdErr)
			{	#Write-Host ("err: type=" + $stdErr.GetType() + "  " + $stdErr)
				if ($stdErr.Contains("warning"))
				{	Write-Host "WARNING: "	-NoNewline -ForegroundColor Yellow	}
				else
				{	Write-Host "ERROR: "		-NoNewline -ForegroundColor Red			}
				Write-Host $stdErr
			}
		}
		else
		{	$stdOut = $InputObject								
			if ($stdOut.Contains("warning"))
			{	Write-Host "WARNING: "	-NoNewline -ForegroundColor Yellow	}
			else
			{	Write-Host "ERROR: "		-NoNewline -ForegroundColor Red			}
			Write-Host $stdOut
		}
	}

	end
	{	$ErrorRecordFound		}
}
