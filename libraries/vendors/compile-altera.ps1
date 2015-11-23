# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	PowerShell Script:	Script to compile the simulation libraries from Altera
#											Quartus-II for GHDL on Windows
# 
#	Authors:						Patrick Lehmann
# 
# Description:
# ------------------------------------
#	This is a PowerShell script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Altera Quartus-II simulation libraries and packages
#
# ==============================================================================
#	Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
#	
#	GHDL is free software; you can redistribute it and/or modify it under
#	the terms of the GNU General Public License as published by the Free
#	Software Foundation; either version 2, or (at your option) any later
#	version.
#	
#	GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
#	WARRANTY; without even the implied warranty of MERCHANTABILITY or
#	FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#	for more details.
#	
#	You should have received a copy of the GNU General Public License
#	along with GHDL; see the file COPYING.  If not, write to the Free
#	Software Foundation, 59 Temple Place - Suite 330, Boston, MA
#	02111-1307, USA.
# ==============================================================================

# .SYNOPSIS
# This CmdLet compiles the simulation libraries from Altera.
# 
# .DESCRIPTION
# This CmdLet:
#   (1) creates a subdirectory in the current working directory
#   (2) compiles all Altera Quartus-II simulation libraries and packages
#       o Altera standard libraries: lpm, sgate, altera, altera_mf, altera_lnsim
#       o Altera device libraries:
#         - arriaii, arriaii_pcie_hip, arriaiigz
#         - arriav, arriavgz, arriavgz_pcie_hip
#         - cycloneiv, cycloneiv_pcie_hip, cycloneive
#         - cyclonev
#         - max, maxii, maxv
#         - stratixiv, stratixiv_pcie_hip
#         - stratixv, stratixv_pcie_hip
#         - fiftyfivenm, twentynm
# 
[CmdletBinding()]
param(
	# Compile all libraries and packages.
	[switch]$All =				$null,
	
	# Compile the Altera standard libraries: lpm, sgate, altera, altera_mf, altera_lnsim
	[switch]$Altera =			$false,
	
	# Compile the Altera Max device libraries
	[switch]$Max =				$false,
	
	# Compile the Altera Cyclon device libraries
	[switch]$Cyclon =			$false,
	
	# Compile the Altera Arria device libraries
	[switch]$Arria =			$false,
	
	# Compile the Altera Stratix device libraries
	[switch]$Stratix =		$false,
	
	# Unknown device library
	[switch]$Nanometer =	$false,
	
	# Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false
)

# ---------------------------------------------
# save working directory
$WorkingDir = Get-Location

# load modules from GHDL's 'vendors' library directory
Import-Module $PSScriptRoot\config.psm1
Import-Module $PSScriptRoot\shared.psm1

# extract data from configuration
$SourceDir =			$InstallationDirectory["AlteraQuartusII"] + "\quartus\eda\sim_lib"
$DestinationDir = $DestinationDirectory["Altera"]

# define global GHDL Options
$GlobalOptions = ("-a", "-fexplicit", "-frelaxed-rules", "--mb-comments", "--warn-binding", "--ieee=synopsys", "--no-vital-checks", "--std=93c")

# create "Altera" directory and change to it
Write-Host "Creating vendor directory: '$DestinationDir'" -ForegroundColor Yellow
mkdir $DestinationDir -ErrorAction SilentlyContinue | Out-Null
cd $DestinationDir

if (-not $All)
{	$All =				$false	}
elseif ($All -eq $true)
{	$Altera =			$true
	$Max =				$true
	$Cyclon =			$true
	$Arria =			$true
	$Stratix =		$true
	$Nanometer =	$true
}

$StopCompiling = $false

# compile lpm library
if ((-not $StopCompiling) -and $Altera)
{	Write-Host "Compiling library 'lpm' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\220pack.vhd",
		"$SourceDir\220model.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=lpm " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile sgate library
if ((-not $StopCompiling) -and $Altera)
{	Write-Host "Compiling library 'sgate' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\sgate_pack.vhd",
		"$SourceDir\sgate.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=sgate " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile altera library
if ((-not $StopCompiling) -and $Altera)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile altera_mf library
if ((-not $StopCompiling) -and $Altera)
{	Write-Host "Compiling library 'altera_mf' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\altera_mf_components.vhd",
		"$SourceDir\altera_mf.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=altera_mf " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile altera_lnsim library
if ((-not $StopCompiling) -and $Altera)
{	Write-Host "Compiling library 'altera_lnsim' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	& ghdl.exe $OPTIONS --work=altera_lnsim $SourceDir\altera_lnsim_components.vhd
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=altera_lnsim " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile max library
if ((-not $StopCompiling) -and $Max)
{	Write-Host "Compiling library 'max' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\max_atoms.vhd",
		"$SourceDir\max_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=max " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile maxii library
if ((-not $StopCompiling) -and $Max)
{	Write-Host "Compiling library 'maxii' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\maxii_atoms.vhd",
		"$SourceDir\maxii_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=maxii " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile maxv library
if ((-not $StopCompiling) -and $Max)
{	Write-Host "Compiling library 'maxv' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\maxv_atoms.vhd",
		"$SourceDir\maxv_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=maxv " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile arriaii library
if ((-not $StopCompiling) -and $Arria)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile arriaii_pcie_hip library
if ((-not $StopCompiling) -and $Arria)
{	Write-Host "Compiling library 'arriaii_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\arriaii_pcie_hip_components.vhd",
		"$SourceDir\arriaii_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=arriaii_pcie_hip " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile arriaiigz library
if ((-not $StopCompiling) -and $Arria)
{	Write-Host "Compiling library 'arriaiigz' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\arriaiigz_atoms.vhd",
		"$SourceDir\arriaiigz_components.vhd",
		"$SourceDir\arriaiigz_hssi_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=arriaiigz " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile arriav library
if ((-not $StopCompiling) -and $Arria)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile arriavgz library
if ((-not $StopCompiling) -and $Arria)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile arriavgz_pcie_hip library
if ((-not $StopCompiling) -and $Arria)
{	Write-Host "Compiling library 'arriavgz_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\arriavgz_pcie_hip_components.vhd",
		"$SourceDir\arriavgz_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=arriavgz_pcie_hip " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile cycloneiv library
if ((-not $StopCompiling) -and $Cyclon)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile cycloneiv_pcie_hip library
if ((-not $StopCompiling) -and $Cyclon)
{	Write-Host "Compiling library 'cycloneiv_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\cycloneiv_pcie_hip_components.vhd",
		"$SourceDir\cycloneiv_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=cycloneiv_pcie_hip " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile cycloneive library
if ((-not $StopCompiling) -and $Cyclon)
{	Write-Host "Compiling library 'cycloneive' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\cycloneive_atoms.vhd",
		"$SourceDir\cycloneive_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=cycloneive " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile cyclonev library
if ((-not $StopCompiling) -and $Cyclon)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile stratixiv library
if ((-not $StopCompiling) -and $Stratix)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile stratixiv_pcie_hip library
if ((-not $StopCompiling) -and $Stratix)
{	Write-Host "Compiling library 'stratixiv_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\stratixiv_pcie_hip_components.vhd",
		"$SourceDir\stratixiv_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=stratixiv_pcie_hip " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile stratixv library
if ((-not $StopCompiling) -and $Stratix)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile stratixv_pcie_hip library
if ((-not $StopCompiling) -and $Stratix)
{	Write-Host "Compiling library 'stratixv_pcie_hip' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\stratixv_pcie_hip_components.vhd",
		"$SourceDir\stratixv_pcie_hip_atoms.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=stratixv_pcie_hip " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile fiftyfivenm library
if ((-not $StopCompiling) -and $Nanometer)
{	Write-Host "Compiling library 'fiftyfivenm' ..." -ForegroundColor Yellow
	$Options = $GlobalOptions
	$Files = (
		"$SourceDir\fiftyfivenm_atoms.vhd",
		"$SourceDir\fiftyfivenm_components.vhd")
	foreach ($File in $Files)
	{	Write-Host "Analysing file '$File'" -ForegroundColor Cyan
		$InvokeExpr = "ghdl.exe " + ($Options -join " ") + " --work=fiftyfivenm " + $File + " 2>&1"
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

# compile twentynm library
if ((-not $StopCompiling) -and $Nanometer)
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
		$ErrorRecordFound = Invoke-Expression $InvokeExpr | Restore-NativeCommandStream | Write-ColoredGHDLLine $SuppressWarnings
		$StopCompiling = ($LastExitCode -ne 0)
		if ($StopCompiling)	{ break }
	}
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling Altera libraries " -NoNewline
if ($StopCompiling)
{	Write-Host "[FAILED]" -ForegroundColor Red				}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green	}

# unload PowerShell modules
Remove-Module shared
Remove-Module config

# restore working directory
cd $WorkingDir

