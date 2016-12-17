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

Write-Host "Configuring GHDL for MinGW64, LLVM-3.5..."

$GHDL_BUILD_DIR =  "$($env:APPVEYOR_BUILD_FOLDER)\build\mingw64-llvm"
$GHDL_PREFIX_DIR = "C:\Tools\GHDL-0.34-dev-mingw64-llvm"

$env:GHDL_BUILD_DIR =  $GHDL_BUILD_DIR
$env:GHDL_PREFIX_DIR = $GHDL_PREFIX_DIR

mkdir $GHDL_BUILD_DIR | cd
c:\msys64\usr\bin\bash.exe -c "../../configure --prefix=$GHDL_PREFIX_DIR --with-llvm-config" 2>&1 | Restore-NativeCommandStream | %{ "$_" }

cd $env:APPVEYOR_BUILD_FOLDER
exit 0
