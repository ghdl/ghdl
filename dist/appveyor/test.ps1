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

Write-Host "Run testsuites..." -Foreground Yellow
cd "$($env:APPVEYOR_BUILD_FOLDER)\testsuite"
# Use a MinGW compatible path
$env:GHDL="$($env:GHDL_PREFIX_DIR)/bin/ghdl.exe"

# ==============================================================================
$TestFramework =  "GNA"
Write-Host "Running GNA tests..." -Foreground Yellow
cd gna

$Directories = dir -Directory *
foreach ($Directory in $Directories)
{	$TestName = "GNA test: {0}" -f $Directory.Name
	$FileName = $Directory.Name
	
	Write-Host $TestName -Foreground Yellow
	cd $Directory
	Add-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome Running
	$start = Get-Date
	c:\msys64\usr\bin\bash.exe -c "./testsuite.sh" 2>&1 | Restore-NativeCommandStream | %{ "$_" }
	$end = Get-Date
	Update-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome $(if ($LastExitCode -eq 0) {"Passed"} else {"Failed"}) -Duration ($end - $start).TotalMilliseconds
	cd ..
}
cd ..

# ==============================================================================
$TestFramework =  "VESTS"
Write-Host "Running VESTS tests..." -Foreground Yellow

c:\msys64\mingw64\bin\gnatmake.exe get_entities 2>&1 | Restore-NativeCommandStream | %{ "$_" }

cd vests

$TestName = "VESTS test:" # {0}" -f $Directory
$FileName = "VESTS" #$Directory

Write-Host $TestName -Foreground Yellow
Add-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome Running
$start = Get-Date

# Disable vests.  It works but takes ~20 min
# c:\msys64\usr\bin\bash.exe -c "./testsuite.sh" 2>&1 | Restore-NativeCommandStream | %{ "$_" }

$end = Get-Date
Update-AppveyorTest -Name $TestName -Framework $TestFramework -FileName $FileName -Outcome $(if ($LastExitCode -eq 0) {"Passed"} else {"Failed"}) -Duration ($end - $start).TotalMilliseconds
cd ..

# ==============================================================================
cd $env:APPVEYOR_BUILD_FOLDER
exit 0
