param(
	[switch]$All =			$null,
	[switch]$unisim =		$false,
	[switch]$simprim =	$false,
	[switch]$secureip =	$false
)

# ---------------------------------------------
# save working directory
$WorkingDir = Get-Location

. $PSScriptRoot\config.ps1
. $PSScriptRoot\shared.ps1

# extract data from configuration
$SourceDir =			$InstallationDirectory["XilinxISE"] + "\ISE_DS\ISE\vhdl\src"
$DestinationDir = $DestinationDirectory["Xilinx"]

# define global GHDL Options
$GlobalOptions = ("-a", "-fexplicit", "-frelaxed-rules", "--warn-binding", "--mb-comments")

# create "Xilinx" directory and change to it
Write-Host "Creating vendor directory: '$DestinationDir'" -ForegroundColor Yellow
mkdir $DestinationDir -ErrorAction SilentlyContinue | Out-Null
cd $DestinationDir

if (-not $All)
{	$All =			$false	}
elseif ($All -eq $true)
{	$unisim =		$true
	$simprim =	$true
	$secureip =	$true
}
$StopCompiling = $false

# compile unisim packages
if ((-not $StopCompiling) -and $unisim)
{	Write-Host "Compiling library 'unisim' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Options += "--no-vital-checks"
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = (
		"$SourceDir\unisims\unisim_VPKG.vhd",
		"$SourceDir\unisims\unisim_VCOMP.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing package '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=unisim " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Format-NativeCommandStreams
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile unisim primitives
if ((-not $StopCompiling) -and $unisim)
{	$Options = $GlobalOptions
	$Options += "--no-vital-checks"
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = dir "$SourceDir\unisims\primitive\*.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=unisim " + $File.FullName + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Format-NativeCommandStreams
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile simprim packages
if ((-not $StopCompiling) -and $simprim)
{	Write-Host "Compiling library 'simprim' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = (
		"$SourceDir\simprims\simprim_Vpackage.vhd",
		"$SourceDir\simprims\simprim_Vcomponents.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing package '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=simprim " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Format-NativeCommandStreams
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile simprim primitives
if ((-not $StopCompiling) -and $simprim)
{	Write-Host "Compiling library 'simprim' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = dir "$SourceDir\simprims\primitive\other\*.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=simprim " + $File.FullName + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Format-NativeCommandStreams
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile secureip primitives
if ((-not $StopCompiling) -and $secureip)
{	Write-Host "Compiling library 'secureip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = dir "$SourceDir\simprims\secureip\other\*.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=secureip " + $File.FullName + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Format-NativeCommandStreams
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling Xilinx libraries " -NoNewline
if ($StopCompiling)
{	Write-Host "[FAILED]" -ForegroundColor Red				}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green	}

# restore working directory
cd $WorkingDir
