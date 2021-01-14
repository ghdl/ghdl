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

#### Environment

$BUILD_DIRNAME = "$($env:BUILD_MINGW)-$($env:BUILD_BACKEND)"
$GHDL_BUILD_DIR =  "$($env:APPVEYOR_BUILD_FOLDER)\build\$BUILD_DIRNAME"

if ($env:APPVEYOR_REPO_TAG -eq "true")
{
  # There is a tag. Remove the leading v.
  $BUILD_VERSION = $($env:APPVEYOR_REPO_TAG_NAME) -creplace "^v", ""
}
else
{
  $BUILD_VERSION = $($env:APPVEYOR_BUILD_VERSION)
}

$PREFIX_DIRNAME = "$BUILD_VERSION-$BUILD_DIRNAME"

$GHDL_PREFIX_DIR = "c:/Tools/GHDL/$PREFIX_DIRNAME"
$ZipFile = "ghdl-$PREFIX_DIRNAME.zip"

$env:GHDL_BUILD_DIR =  $GHDL_BUILD_DIR
$env:GHDL_PREFIX_DIR = $GHDL_PREFIX_DIR

#### Build

mkdir $GHDL_BUILD_DIR | cd

if ($env:BUILD_BACKEND -eq "mcode")
{	Write-Host "Configuring GHDL for $($env:BUILD_MINGW), mcode..." -ForegroundColor Yellow

	c:\msys64\usr\bin\bash.exe -c "../../configure --prefix=$GHDL_PREFIX_DIR LDFLAGS=-static" 2>&1 | Restore-NativeCommandStream | %{ "$_" }
}
elseif ($env:BUILD_BACKEND -eq "llvm")
{	Write-Host "Configuring GHDL for $($env:BUILD_MINGW), LLVM..." -ForegroundColor Yellow

	c:\msys64\usr\bin\bash.exe -c "../../configure --prefix=$GHDL_PREFIX_DIR --with-llvm-config='llvm-config --link-static' LDFLAGS='-static' --enable-libghdl --enable-synth CXX=clang++" 2>&1 | Restore-NativeCommandStream | %{ "$_" }
}

Write-Host "Building GHDL and libraries..." -ForegroundColor Yellow
c:\msys64\usr\bin\make.exe 2>&1 | Restore-NativeCommandStream | %{ "$_" }
$Err = $LastExitCode

if ($Err -eq 0)
{
  Write-Host "Installing GHDL and libraries..." -ForegroundColor Yellow
  c:\msys64\usr\bin\make.exe install 2>&1 | Restore-NativeCommandStream | %{ "$_" }
  $Err = $LastExitCode
}

#### Binaries

if ($Err -eq 0)
{
  Write-Host "Building binary archives..." -ForegroundColor Yellow
  cd c:\Tools
  7z a "$($env:APPVEYOR_BUILD_FOLDER)\$ZipFile" -r "GHDL\$PREFIX_DIRNAME\"

  cd $env:APPVEYOR_BUILD_FOLDER
  Push-AppveyorArtifact $ZipFile
}

cd $env:APPVEYOR_BUILD_FOLDER

exit $Err
