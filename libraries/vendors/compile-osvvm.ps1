param(
	[switch]$All =							$true,
	
	[switch]$SuppressWarnings = $false
)

# ---------------------------------------------
# save working directory
$WorkingDir = Get-Location

. $PSScriptRoot\config.ps1
. $PSScriptRoot\shared.ps1

# extract data from configuration
$SourceDir =			$InstallationDirectory["OSVVM"]
$DestinationDir = $DestinationDirectory["OSVVM"]

# define global GHDL Options
$GlobalOptions = ("-a", "-fexplicit", "-frelaxed-rules", "--mb-comments", "--warn-binding", "--no-vital-checks", "--std=08")

# create "osvvm" directory and change to it
Write-Host "Creating vendor directory: '$DestinationDir'" -ForegroundColor Yellow
mkdir $DestinationDir -ErrorAction SilentlyContinue | Out-Null
cd $DestinationDir

if (-not $All)
{	$All =				$false	}
elseif ($All -eq $true)
{	# nothing to configure
}

$StopCompiling = $false

# compile osvvm library
if (-not $StopCompiling)
{	Write-Host "Compiling library 'osvvm' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\NamePkg.vhd",
		"$SourceDir\OsvvmGlobalPkg.vhd",
		"$SourceDir\TextUtilPkg.vhd",
		"$SourceDir\TranscriptPkg.vhd",
		"$SourceDir\AlertLogPkg.vhd",
		"$SourceDir\MemoryPkg.vhd",
		"$SourceDir\MessagePkg.vhd",
		"$SourceDir\SortListPkg_int.vhd",
		"$SourceDir\RandomBasePkg.vhd",
		"$SourceDir\RandomPkg.vhd",
		"$SourceDir\CoveragePkg.vhd",
		"$SourceDir\OsvvmContext.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing package '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=simprim " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Collect-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling OSVVM libraries " -NoNewline
if ($StopCompiling)
{	Write-Host "[FAILED]" -ForegroundColor Red				}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green	}

# restore working directory
cd $WorkingDir
