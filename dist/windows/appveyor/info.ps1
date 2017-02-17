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


Write-Host ("ExecutionPolicy = {0}" -f (Get-ExecutionPolicy)) -Foreground Yellow
Write-Host "List env:..." -Foreground Yellow
dir env: | foreach { Write-Host ("  {0}={1}" -f $_.Name,$_.Value) }
Write-Host "Print env:PATH..." -Foreground Yellow
$env:PATH.Split(";") | foreach { Write-Host "  $_" }
Write-Host "Print GCC setup..." -Foreground Yellow
gcc.exe -v 2>&1 | Restore-NativeCommandStream | %{ "$_" }
Write-Host "Print GCC search directories..." -Foreground Yellow
gcc.exe -print-search-dirs 2>&1 | Restore-NativeCommandStream | %{ "$_" }

if ($env:BUILD_BACKEND -eq "llvm")
{	Write-Host "Print CLang setup..." -Foreground Yellow
	clang.exe -v 2>&1 | Restore-NativeCommandStream | %{ "$_" }
	Write-Host "Print CLang search directories..." -Foreground Yellow
	clang.exe -print-search-dirs 2>&1 | Restore-NativeCommandStream | %{ "$_" }
}

Write-Host "Print gnatls setup..." -Foreground Yellow
gnatls.exe -v 2>&1 | Restore-NativeCommandStream | %{ "$_" }
