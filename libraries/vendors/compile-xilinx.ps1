param(
	[switch]$All =			$null,
	[switch]$unisim =		$false,
	[switch]$simprim =	$false
)

# configure Xilinx folder here
$SourceDir = "C:\Xilinx\14.7\ISE_DS\ISE\vhdl\src"
$DestinationDirectory = "xilinx"

# ---------------------------------------------
# save working directory
$WorkingDir = Get-Location

# define global GHDL Options
$GlobalOptions = ("-a", "-fexplicit", "-frelaxed-rules", "--warn-binding", "--mb-comments")

# create "Xilinx" directory and change to it
Write-Host "Creating vendor directory: '$DestinationDirectory'" -ForegroundColor Yellow
mkdir $DestinationDirectory -ErrorAction SilentlyContinue | Out-Null
cd $DestinationDirectory

if (-not $All)
{	$All =			$false	}
elseif ($All -eq $true)
{	$unisim =		$true
	$simprim =	$true
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

# compile unisim primitives
if ((-not $StopCompiling) -and $unisim)
{	$Files = dir "$SourceDir\unisims\primitive\*.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=unisim " + $File.FullName + " 2>&1"
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
	}
}

# compile simprim packages
if ((-not $StopCompiling) -and $simprim)
{	Write-Host "Compiling library 'simprim' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Options += "--ieee=synopsys"
	$Options += "--std=02"
	$Files = (
		"$SourceDir\simprims\simprim_Vpackage.vhd",
		"$SourceDir\simprims\simprim_Vpackage_mti.vhd",
		"$SourceDir\simprims\simprim_Vcomponents.vhd",
		"$SourceDir\simprims\simprim_Vcomponents_mti.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing package '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=simprim " + $File + " 2>&1"
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

# compile simprim primitives
if ((-not $StopCompiling) -and $simprim)
{	$Files = dir "$SourceDir\simprims\primitive\mti\*.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=simprim " + $File.FullName + " 2>&1"
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
	}
}

# compile simprim primitives
if ((-not $StopCompiling) -and $simprim)
{	$Files = dir "$SourceDir\simprims\primitive\other\*.vhd*"
	foreach ($File in $Files)
	{	Write-Host "Analysing primitive '$($File.FullName)'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=simprim " + $File.FullName + " 2>&1"
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
		$i -= 1
		if ($i -eq 0)	{	break	}
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
