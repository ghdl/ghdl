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

Write-Host "Installing GHDL and libraries..." -Foreground Yellow
cd $env:GHDL_BUILD_DIR
c:\msys64\usr\bin\make.exe install 2>&1 | Restore-NativeCommandStream | %{ "$_" }

cd c:\Tools
7z a "$($env:APPVEYOR_BUILD_FOLDER)\ghdl-0.34-dev-$($env:BUILD_MINGW)-$($env:BUILD_BACKEND).zip" -r "GHDL\0.34-dev-$($env:BUILD_MINGW)-$($env:BUILD_BACKEND)\"

cd $env:APPVEYOR_BUILD_FOLDER
Push-AppveyorArtifact "ghdl-0.34-dev-$($env:BUILD_MINGW)-$($env:BUILD_BACKEND).zip"

exit 0
