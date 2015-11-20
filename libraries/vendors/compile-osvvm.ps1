param(
	[switch]$All =				$true
)

# configure OSVVM folder here
$SourceDir = "D:\git\PoC\lib\osvvm"
$DestinationDirectory = "osvvm"

# ---------------------------------------------
# save working directory
$WorkingDir = Get-Location

# define global GHDL Options
$GlobalOptions = ("-a", "-fexplicit", "-frelaxed-rules", "--mb-comments", "--warn-binding", "--no-vital-checks", "--std=08")

# create "osvvm" directory and change to it
Write-Host "Creating vendor directory: '$DestinationDirectory'" -ForegroundColor Yellow
mkdir $DestinationDirectory -ErrorAction SilentlyContinue | Out-Null
cd $DestinationDirectory

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
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=osvvm " + $File + " 2>&1"
		#Write-Host ("InvokeExpr=" + $InvokeExpr)
			
		$Output = Invoke-Expression $InvokeExpr -ErrorVariable Errors | Out-Null
		$StopCompiling = ($LastExitCode -ne 0)
		if ($Errors)
		{	$Line = $Errors[0].ToString()
			if ($Line.Contains("warning"))
			{	Write-Host "WARNING: $Line"	}
			else
			{	Write-Host "ERROR: $Line"		}
		}
		elseif ($Output)
		{	Write-Host ("Output.Type=" + $Output.GetType())
			foreach ($Line in $Output)
			{	if ($Line -eq "")	{	continue	}
				if ($Line.Contains("warning"))
				{	Write-Host "WARNING: $Line"	}
				else
				{	Write-Host "ERROR: $Line"		}
			}
		}
		if ($StopCompiling)	{ break		}
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
