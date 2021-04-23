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

Write-Host "Run testsuites..." -ForegroundColor Yellow
cd "$($env:APPVEYOR_BUILD_FOLDER)\testsuite"
# Use a MinGW compatible path
$env:GHDL="$($env:GHDL_PREFIX_DIR)/bin/ghdl.exe"
$env:GHWDUMP="$($env:GHDL_PREFIX_DIR)/bin/ghwdump.exe"

# Exit status
$Err = 0

# =============================================================================
$TestFramework =  "GNA"
Write-Host "Running GNA tests..." -ForegroundColor Yellow
cd gna

$Directories = dir -Directory *
foreach ($Directory in $Directories)
{	$TestName = "GNA test: {0}" -f $Directory.Name
	$FileName = $Directory.Name

	Write-Host $TestName -ForegroundColor Yellow
	cd $Directory
	Add-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome Running
	$start = Get-Date
	c:\msys64\usr\bin\bash.exe -c "./testsuite.sh" 2>&1 | Restore-NativeCommandStream | %{ "$_" }
	$end = Get-Date
	$TotalMilliseconds = ($end - $start).TotalMilliseconds
	if ($LastExitCode -eq 0)
	{ Write-Host "PASSED" -ForegroundColor Green
		Update-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome Passed -Duration $TotalMilliseconds
	}
	else
	{	Write-Host "FAILED" -ForegroundColor Red
		Update-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome Failed -Duration $TotalMilliseconds
		$Err = 1
	}
}
cd ..\..

# =============================================================================
$TestFramework =  "VESTS"
Write-Host "Running VESTS tests..." -ForegroundColor Yellow

cd vests

$TestName = "VESTS test:" # {0}" -f $Directory
$FileName = "VESTS" #$Directory

Write-Host $TestName -ForegroundColor Yellow
# Disable vests.  It works but takes ~20 min
if ($true)
{	Add-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome Skipped
	$start = Get-Date
}
else
{	Add-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome Running
	$start = Get-Date
	c:\msys64\usr\bin\bash.exe -c "./testsuite.sh" 2>&1 | Restore-NativeCommandStream | %{ "$_" }
	$end = Get-Date
	$TotalMilliseconds = ($end - $start).TotalMilliseconds
	if ($LastExitCode -eq 0)
	{ Write-Host "PASSED" -ForegroundColor Green
		Update-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome Passed -Duration $TotalMilliseconds
	}
	else
	{	Write-Host "FAILED" -ForegroundColor Red
		Update-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome Failed -Duration $TotalMilliseconds
		$Err = 1
	}
	cd ..
}

# =============================================================================
cd $env:APPVEYOR_BUILD_FOLDER
exit $Err
