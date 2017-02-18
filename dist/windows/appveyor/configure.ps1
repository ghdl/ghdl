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

cd $env:APPVEYOR_BUILD_FOLDER

# Assemble paths
$GHDL_BUILD_DIR =  "$($env:APPVEYOR_BUILD_FOLDER)\build\$($env:BUILD_MINGW)-$($env:BUILD_BACKEND)"
$GHDL_PREFIX_DIR = "/c/Tools/GHDL/0.34-dev-$($env:BUILD_MINGW)-$($env:BUILD_BACKEND)"
$GCC_BUILD_DIR =   "$($env:APPVEYOR_BUILD_FOLDER)\gcc-$($env:BUILD_GCC_VERSION)\build-$($env:BUILD_MINGW)"
# Export paths as environment variable
$env:GHDL_BUILD_DIR =  $GHDL_BUILD_DIR
$env:GHDL_PREFIX_DIR = $GHDL_PREFIX_DIR
# Create GHDL build directory
mkdir $GHDL_BUILD_DIR | cd

if ($env:BUILD_BACKEND -eq "mcode")
{	Write-Host "Configuring GHDL for $($env:BUILD_MINGW), mcode..." -Foreground Yellow

	c:\msys64\usr\bin\bash.exe -c "../../configure --prefix=$GHDL_PREFIX_DIR LDFLAGS=-static" 2>&1 | Restore-NativeCommandStream | %{ "$_" }
}
elseif ($env:BUILD_BACKEND -eq "llvm")
{	Write-Host "Configuring GHDL for $($env:BUILD_MINGW), LLVM-3.5..." -Foreground Yellow
	c:\msys64\usr\bin\bash.exe -c "../../configure --prefix=$GHDL_PREFIX_DIR --with-llvm-config LDFLAGS=-static CXX=g++" 2>&1 | Restore-NativeCommandStream | %{ "$_" }
}
elseif ($env:BUILD_BACKEND -eq "gcc")
{	# Export GCC paths as environment variable
	$env:GCC_BUILD_DIR =   $GCC_BUILD_DIR
	# Create GCC build directory
	mkdir $GCC_BUILD_DIR
	
	Write-Host "Configuring GHDL for $($env:BUILD_MINGW), GCC $($env:BUILD_GCC_VERSION)..." -Foreground Yellow
	c:\msys64\usr\bin\bash.exe -c "../../configure --prefix=$GHDL_PREFIX_DIR --with-gcc=$($env:GCC_SOURCE_DIR) LDFLAGS=-static CXX=g++" 2>&1 | Restore-NativeCommandStream | %{ "$_" }
	
	Write-Host "Copy GHDL sources into GCC as a new language (vhdl)..." -Foreground Yellow
	c:\msys64\usr\bin\make.exe copy-sources 2>&1 | Restore-NativeCommandStream | %{ "$_" }
}

cd $env:APPVEYOR_BUILD_FOLDER
exit $LastExitCode
