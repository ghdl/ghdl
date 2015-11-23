param(
	[switch]$All =			$null,
	[switch]$Unisim =		$false,
	[switch]$Unimacro =	$false,
	[switch]$Secureip =	$false,
	
	[switch]$SuppressWarnings = $false
)

# ---------------------------------------------
# save working directory
$WorkingDir = Get-Location

. $PSScriptRoot\config.ps1
. $PSScriptRoot\shared.ps1

# extract data from configuration
$SourceDir =			$InstallationDirectory["XilinxVivado"] + "\data\vhdl\src"
$DestinationDir = $DestinationDirectory["XilinxVivado"]

# define global GHDL Options
$GlobalOptions = ("-a", "-fexplicit", "-frelaxed-rules", "--warn-binding", "--mb-comments")

# create "Vivado" directory and change to it
Write-Host "Creating vendor directory: '$DestinationDir'" -ForegroundColor Yellow
mkdir $DestinationDir -ErrorAction SilentlyContinue | Out-Null
cd $DestinationDir

if (-not $All)
{	$All =			$false	}
elseif ($All -eq $true)
{	$Unisim =		$true
	$Simprim =	$true
	$Unimacro =	$true
	$Secureip =	$true
}
$StopCompiling = $false

# Library UNISIM
# ==============================================================================
# compile unisim packages
if ((-not $StopCompiling) -and $Unisim)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Collect-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile unisim primitives
if ((-not $StopCompiling) -and $Unisim)
{	$Options = $GlobalOptions
	$Options += "--no-vital-checks"
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = dir "$SourceDir\unisims\primitive\*.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=unisim " + $File.FullName + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Collect-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile unisim retarget primitives
if ((-not $StopCompiling) -and $Unisim)
{	$Options = $GlobalOptions
	$Options += "--no-vital-checks"
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = dir "$SourceDir\unisims\retarget\*.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing retarget primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=unisim " + $File.FullName + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Collect-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		#if ($StopCompiling)	{ break }
	}
}

# compile unisim secureip primitives
if ((-not $StopCompiling) -and $Unisim -and $Secureip)
{	Write-Host "Compiling library secureip primitives ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = dir "$SourceDir\unisims\secureip\*.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=secureip " + $File.FullName + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Collect-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# Library UNIMACRO
# ==============================================================================
# compile unimacro packages
if ((-not $StopCompiling) -and $Unimacro)
{	Write-Host "Compiling library 'unimacro' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Options += "--no-vital-checks"
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = @(
		"$SourceDir\unimacro\unimacro_VCOMP.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing package '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=unimacro " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Collect-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile unimacro macros
if ((-not $StopCompiling) -and $Unimacro)
{	$Options = $GlobalOptions
	$Options += "--no-vital-checks"
	$Options += "--ieee=synopsys"
	$Options += "--std=93c"
	$Files = dir "$SourceDir\unimacro\*_MACRO.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=unimacro " + $File.FullName + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Collect-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		#if ($StopCompiling)	{ break }
	}
}

# Library UNIFAST
# ==============================================================================
# TODO:

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling Xilinx Vivado libraries " -NoNewline
if ($StopCompiling)
{	Write-Host "[FAILED]" -ForegroundColor Red				}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green	}

# restore working directory
cd $WorkingDir
