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
$env:MSYS2_ARG_CONV_EXC = "*"

if ($env:BUILD_BACKEND -in @("mcode", "llvm"))
{	Write-Host "Building GHDL and libraries..." -Foreground Yellow
	cd $env:GHDL_BUILD_DIR
	c:\msys64\usr\bin\make.exe 2>&1 | Restore-NativeCommandStream | %{ "$_" }
	
	Write-Host "Installing GHDL and libraries..." -Foreground Yellow
	c:\msys64\usr\bin\make.exe install 2>&1 | Restore-NativeCommandStream | %{ "$_" }
}
elseif ($env:BUILD_BACKEND -eq "gcc")
{	Write-Host "Configuring GCC with GHDL frontend..." -Foreground Yellow
	cd $env:GCC_BUILD_DIR
	c:\msys64\usr\bin\bash.exe -c "../configure --prefix=$($env:GHDL_PREFIX_DIR) --enable-languages=c,vhdl --disable-bootstrap --build=x86_64-w64-mingw32 --host=x86_64-w64-mingw32 --target=x86_64-w64-mingw32 --with-native-system-header-dir=/mingw64/include --disable-lto --disable-multilib --disable-libssp --disable-libgomp --disable-libquadmath" 2>&1 | Restore-NativeCommandStream | %{ "$_" }

	Write-Host "Building GCC with GHDL frontend..." -Foreground Yellow
	c:\msys64\usr\bin\make.exe 2>&1 | Restore-NativeCommandStream | %{ "$_" }
	
	Write-Host "Installing GHDL ..." -Foreground Yellow
	c:\msys64\usr\bin\make.exe install 2>&1 | Restore-NativeCommandStream | %{ "$_" }
	
	Write-Host "Building GHDL libraries..." -Foreground Yellow
	cd $env:GHDL_BUILD_DIR
	c:\msys64\usr\bin\make.exe ghdllib 2>&1 | Restore-NativeCommandStream | %{ "$_" }
	
	Write-Host "Installing GHDL libraries..." -Foreground Yellow
	c:\msys64\usr\bin\make.exe install 2>&1 | Restore-NativeCommandStream | %{ "$_" }
}

cd $env:APPVEYOR_BUILD_FOLDER
exit 0
