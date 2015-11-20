param(
	[switch]$All =				$null,
	[switch]$max =				$false,
	[switch]$cyclon =			$false,
	[switch]$arria =			$false,
	[switch]$stratix =		$false,
	[switch]$nanometer =	$false
)

# configure Altera folder here
$SourceDir = "C:\Altera\15.0\quartus\eda\sim_lib"
$DestinationDirectory = "altera"

# ---------------------------------------------
# save working directory
$WorkingDir = Get-Location

# define global GHDL Options
$GlobalOptions = ("-a", "-fexplicit", "-frelaxed-rules", "--mb-comments", "--warn-binding", "--ieee=synopsys", "--no-vital-checks", "--std=93c")

# create "Altera" directory and change to it
Write-Host "Creating vendor directory: '$DestinationDirectory'" -ForegroundColor Yellow
mkdir $DestinationDirectory -ErrorAction SilentlyContinue | Out-Null
cd $DestinationDirectory

if (-not $All)
{	$All =				$false	}
elseif ($All -eq $true)
{	$max =				$true
	$cyclon =			$true
	$arria =			$true
	$stratix =		$true
	$nanometer =	$true
}

$StopCompiling = $false

# compile lpm library
if (-not $StopCompiling)
{	Write-Host "Compiling library 'lpm' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\220pack.vhd",
		"$SourceDir\220model.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=lpm " + $File + " 2>&1"
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

# compile sgate library
if (-not $StopCompiling)
{	Write-Host "Compiling library 'sgate' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\sgate_pack.vhd",
		"$SourceDir\sgate.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=sgate " + $File + " 2>&1"
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

# compile altera library
if (-not $StopCompiling)
{	Write-Host "Compiling library 'altera' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\altera_europa_support_lib.vhd",
		"$SourceDir\altera_mf_components.vhd",
		"$SourceDir\altera_mf.vhd",
		"$SourceDir\altera_primitives_components.vhd",
		"$SourceDir\altera_primitives.vhd",
		"$SourceDir\altera_standard_functions.vhd",
		"$SourceDir\altera_syn_attributes.vhd",
		"$SourceDir\alt_dspbuilder_package.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=altera " + $File + " 2>&1"
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

# compile altera_mf library
if (-not $StopCompiling)
{	Write-Host "Compiling library 'altera_mf' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\altera_mf_components.vhd",
		"$SourceDir\altera_mf.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=altera_mf " + $File + " 2>&1"
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

# compile altera_lnsim library
if (-not $StopCompiling)
{	Write-Host "Compiling library 'altera_lnsim' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	& ghdl.exe $OPTIONS --work=altera_lnsim $SourceDir\altera_lnsim_components.vhd
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=altera_lnsim " + $File + " 2>&1"
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

# compile arriaii library
if ((-not $StopCompiling) -and $arria)
{	Write-Host "Compiling library 'arriaii' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\arriaii_atoms.vhd",
		"$SourceDir\arriaii_components.vhd",
		"$SourceDir\arriaii_hssi_components.vhd",
		"$SourceDir\arriaii_hssi_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=arriaii " + $File + " 2>&1"
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

# compile arriaii_pcie_hip library
if ((-not $StopCompiling) -and $arria)
{	Write-Host "Compiling library 'arriaii_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\arriaii_pcie_hip_components.vhd",
		"$SourceDir\arriaii_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=arriaii_pcie_hip " + $File + " 2>&1"
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

# compile arriaiigz library
if ((-not $StopCompiling) -and $arria)
{	Write-Host "Compiling library 'arriaiigz' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\arriaiigz_atoms.vhd",
		"$SourceDir\arriaiigz_components.vhd",
		"$SourceDir\arriaiigz_hssi_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=arriaiigz " + $File + " 2>&1"
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

# compile arriav library
if ((-not $StopCompiling) -and $arria)
{	Write-Host "Compiling library 'arriav' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\arriav_atoms.vhd",
		"$SourceDir\arriav_components.vhd",
		"$SourceDir\arriav_hssi_components.vhd",
		"$SourceDir\arriav_hssi_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=arriav " + $File + " 2>&1"
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

# compile arriavgz library
if ((-not $StopCompiling) -and $arria)
{	Write-Host "Compiling library 'arriavgz' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\arriavgz_atoms.vhd",
		"$SourceDir\arriavgz_components.vhd",
		"$SourceDir\arriavgz_hssi_components.vhd",
		"$SourceDir\arriavgz_hssi_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=arriavgz " + $File + " 2>&1"
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

# compile arriavgz_pcie_hip library
if ((-not $StopCompiling) -and $arria)
{	Write-Host "Compiling library 'arriavgz_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\arriavgz_pcie_hip_components.vhd",
		"$SourceDir\arriavgz_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=arriavgz_pcie_hip " + $File + " 2>&1"
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

# compile cycloneiv library
if ((-not $StopCompiling) -and $cyclon)
{	Write-Host "Compiling library 'cycloneiv' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\cycloneiv_atoms.vhd",
		"$SourceDir\cycloneiv_components.vhd",
		"$SourceDir\cycloneiv_hssi_components.vhd",
		"$SourceDir\cycloneiv_hssi_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=cycloneiv " + $File + " 2>&1"
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

# compile cycloneiv_pcie_hip library
if ((-not $StopCompiling) -and $cyclon)
{	Write-Host "Compiling library 'cycloneiv_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\cycloneiv_pcie_hip_components.vhd",
		"$SourceDir\cycloneiv_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=cycloneiv_pcie_hip " + $File + " 2>&1"
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

# compile cycloneive library
if ((-not $StopCompiling) -and $cyclon)
{	Write-Host "Compiling library 'cycloneive' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\cycloneive_atoms.vhd",
		"$SourceDir\cycloneive_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=cycloneive " + $File + " 2>&1"
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

# compile cyclonev library
if ((-not $StopCompiling) -and $cyclon)
{	Write-Host "Compiling library 'cyclonev' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\cyclonev_atoms.vhd",
		"$SourceDir\cyclonev_components.vhd",
		"$SourceDir\cyclonev_hssi_components.vhd",
		"$SourceDir\cyclonev_hssi_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=cyclonev " + $File + " 2>&1"
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

# compile max library
if ((-not $StopCompiling) -and $max)
{	Write-Host "Compiling library 'max' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\max_atoms.vhd",
		"$SourceDir\max_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=max " + $File + " 2>&1"
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

# compile maxii library
if ((-not $StopCompiling) -and $max)
{	Write-Host "Compiling library 'maxii' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\maxii_atoms.vhd",
		"$SourceDir\maxii_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=maxii " + $File + " 2>&1"
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

# compile maxv library
if ((-not $StopCompiling) -and $max)
{	Write-Host "Compiling library 'maxv' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\maxv_atoms.vhd",
		"$SourceDir\maxv_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=maxv " + $File + " 2>&1"
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

# compile stratixiv library
if ((-not $StopCompiling) -and $stratix)
{	Write-Host "Compiling library 'stratixiv' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\stratixiv_atoms.vhd",
		"$SourceDir\stratixiv_components.vhd",
		"$SourceDir\stratixiv_hssi_components.vhd",
		"$SourceDir\stratixiv_hssi_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=stratixiv " + $File + " 2>&1"
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

# compile stratixiv_pcie_hip library
if ((-not $StopCompiling) -and $stratix)
{	Write-Host "Compiling library 'stratixiv_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\stratixiv_pcie_hip_components.vhd",
		"$SourceDir\stratixiv_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=stratixiv_pcie_hip " + $File + " 2>&1"
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

# compile stratixv library
if ((-not $StopCompiling) -and $stratix)
{	Write-Host "Compiling library 'stratixv' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\stratixv_atoms.vhd",
		"$SourceDir\stratixv_components.vhd",
		"$SourceDir\stratixv_hssi_components.vhd",
		"$SourceDir\stratixv_hssi_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=stratixv " + $File + " 2>&1"
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

# compile stratixv_pcie_hip library
if ((-not $StopCompiling) -and $stratix)
{	Write-Host "Compiling library 'stratixv_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\stratixv_pcie_hip_components.vhd",
		"$SourceDir\stratixv_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=stratixv_pcie_hip " + $File + " 2>&1"
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

# compile fiftyfivenm library
if ((-not $StopCompiling) -and $nanometer)
{	Write-Host "Compiling library 'fiftyfivenm' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\fiftyfivenm_atoms.vhd",
		"$SourceDir\fiftyfivenm_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=fiftyfivenm " + $File + " 2>&1"
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

# compile twentynm library
if ((-not $StopCompiling) -and $nanometer)
{	Write-Host "Compiling library 'twentynm' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\twentynm_atoms.vhd",
		"$SourceDir\twentynm_components.vhd",
		"$SourceDir\twentynm_hip_components.vhd",
		"$SourceDir\twentynm_hip_atoms.vhd",
		"$SourceDir\twentynm_hssi_components.vhd",
		"$SourceDir\twentynm_hssi_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=twentynm " + $File + " 2>&1"
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
