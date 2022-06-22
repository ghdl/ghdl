# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
# ==============================================================================
#  Copyright (C) 2017-2021 Patrick Lehmann - Boetzingen, Germany
#  Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <gnu.org/licenses>.
# ==============================================================================

# .SYNOPSIS
# Script to compile the simulation libraries from Intel  Quartus for GHDL on Windows.
#
# .DESCRIPTION
# This CmdLet:
#   (1) creates a subdirectory in the current working directory
#   (2) compiles all Altera Quartus simulation libraries and packages
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
	# Show the embedded help page(s)
	[switch]$Help =       $false,

	# Compile all libraries and packages.
	[switch]$All =        $false,

	# Compile the Altera standard libraries: lpm, sgate, altera, altera_mf, altera_lnsim
	[switch]$Altera =     $false,

	# Compile the Altera Max device libraries
	[switch]$Max =        $false,

	# Compile the Altera Cyclone device libraries
	[switch]$Cyclone =    $false,

	# Compile the Altera Arria device libraries
	[switch]$Arria =      $false,

	# Compile the Altera Stratix device libraries
	[switch]$Stratix =    $false,

	# Unknown device library
	[switch]$Nanometer =  $false,

	# Clean up directory before analyzing.
	[switch]$Clean =      $false,

	# Set VHDL Standard to '93.
	[switch]$VHDL93 =     $false,
	# Set VHDL Standard to '08.
	[switch]$VHDL2008 =   $false,

	# Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false,
	# Halt on errors.
	[switch]$HaltOnError =      $false,

	# Set vendor library source directory.
	[string]$Source =     "",
	# Set output directory name.
	[string]$Output =     "",
	# Set path to GHDL's executable, e.g. <MyGHDLPath>/bin/ghdl.exe
	[string]$GHDL =       ""
)

# ---------------------------------------------
# save working directory
$WorkingDir =     Get-Location

# set default values
$EnableDebug =    [bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
$EnableVerbose =  [bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug

# load modules from GHDL's 'vendors' library directory
$EnableVerbose -and  (Write-Host "Loading modules..." -ForegroundColor Gray  ) | Out-Null
$EnableDebug -and    (Write-Host "  Import-Module $PSScriptRoot\config.psm1 -Verbose:`$$false -Debug:`$$false -ArgumentList `"IntelQuartus`"" -ForegroundColor DarkGray  ) | Out-Null
Import-Module $PSScriptRoot\config.psm1 -Verbose:$false -Debug:$false -ArgumentList "IntelQuartus"
$EnableDebug -and    (Write-Host "  Import-Module $PSScriptRoot\shared.psm1 -Verbose:`$$false -Debug:`$$false -ArgumentList @(`"Intel Quartus Prime`", `"$WorkingDir`")" -ForegroundColor DarkGray  ) | Out-Null
Import-Module $PSScriptRoot\shared.psm1 -Verbose:$false -Debug:$false -ArgumentList @("Intel Quartus Prime", "$WorkingDir")

# Display help if no command was selected
$Help = $Help -or (-not ($All -or $Altera -or $Max -or $Cyclone -or $Arria -or $Stratix -or $Nanometer -or $Clean))

if ($Help)
{	Get-Help $MYINVOCATION.MyCommand.Path -Detailed
	Exit-CompileScript
}
if ($All)
{	$Altera =      $true
	$Max =        $true
	$Cyclone =    $true
	$Arria =      $true
	$Stratix =    $true
	$Nanometer =  $true
}

function Get-AlteraQuartusDirectory
{	if (Test-Path env:QUARTUS_ROOTDIR)
	{	return $env:QUARTUS_ROOTDIR + "\" + (Get-VendorToolSourceDirectory)    }
	else
	{	$EnvSourceDir = ""
		foreach ($Drive in Get-PSDrive -PSProvider 'FileSystem')
		{	$Path = $Drive.Name + ":\" + "Altera"
			if (Test-Path $Path -PathType Container)
			{	foreach ($Major in 21..13)
				{	foreach ($Minor in 5..0)
					{	$Dir = $Path + "\" + $Major + "." + $Minor + "\quartus"
						if (Test-Path $Dir -PathType Container)
						{	$EnvSourceDir = $Dir + "\" + (Get-VendorToolSourceDirectory)
							return $EnvSourceDir
						}
					}
				}
			}
		}
	}
}

$SourceDirectory =      Get-SourceDirectory $Source (Get-AlteraQuartusDirectory)
$DestinationDirectory =  Get-DestinationDirectory $Output
$GHDLBinary =            Get-GHDLBinary $GHDL

# create "Altera" directory and change to it
New-DestinationDirectory $DestinationDirectory
cd $DestinationDirectory


$VHDLVersion,$VHDLStandard,$VHDLFlavor = Get-VHDLVariables -VHDL93:$VHDL93 -VHDL2008:$VHDL2008

# define global GHDL Options
$Analyze_Parameters = @(
	"--std=$VHDLStandard",
	"-fexplicit",
	"-frelaxed-rules",
	"--mb-comments",
	"-Wbinding"
)
if (-not $EnableDebug)
{	$Analyze_Parameters += @(
		"-Wno-hide"
	)
}
if (-not ($EnableVerbose -or $EnableDebug))
{ $Analyze_Parameters += @(
		"-Wno-library",
		"-Wno-others",
		"-Wno-static"
	)
}
$Analyze_Parameters += @(
	"--ieee=$VHDLFlavor",
	"--no-vital-checks",
	"-P$DestinationDirectory"
)

# extract data from configuration
# $SourceDir =      $InstallationDirectory["AlteraQuartus"] + "\quartus\eda\sim_lib"

$StopCompiling =  $false
$ErrorCount =     0

# Cleanup directories
# ==============================================================================
if ($Clean)
{	Write-Host "[ERROR]: '-Clean' is not implemented!" -ForegroundColor Red
	Exit-CompileScript -1

	Write-Host "Cleaning up vendor directory ..." -ForegroundColor Yellow
	rm *.cf
}


# Altera standard libraries
# ==============================================================================
# compile lpm library
if ((-not $StopCompiling) -and $Altera)
{	$Library = "lpm"
	$Files = @(
		"220pack.vhd",
		"220model.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# compile sgate library
if ((-not $StopCompiling) -and $Altera)
{	$Library = "sgate"
	$Files = @(
		"sgate_pack.vhd",
		"sgate.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# compile altera library
if ((-not $StopCompiling) -and $Altera)
{	$Library = "altera"
	$Files = @(
		"altera_europa_support_lib.vhd",
		"altera_primitives_components.vhd",
		"altera_primitives.vhd",
		"altera_standard_functions.vhd",
		"altera_syn_attributes.vhd",
		"alt_dspbuilder_package.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# compile altera_mf library
if ((-not $StopCompiling) -and $Altera)
{	$Library = "altera_mf"
	$Files = @(
		"altera_mf_components.vhd",
		"altera_mf.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# compile altera_lnsim library
if ((-not $StopCompiling) -and $Altera)
{	$Library = "altera_lnsim"
	$Files = @(
		"altera_lnsim_components.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Altera device libraries
# ==============================================================================
# compile max library
if ((-not $StopCompiling) -and $Max)
{	$Library = "max"
	$Files = @(
		"max_atoms.vhd",
		"max_components.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile maxii library
if ((-not $StopCompiling) -and $Max)
{	$Library = "maxii"
	$Files = @(
		"maxii_atoms.vhd",
		"maxii_components.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile maxv library
if ((-not $StopCompiling) -and $Max)
{	$Library = "maxv"
	$Files = @(
		"maxv_atoms.vhd",
		"maxv_components.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile arriaii library
if ((-not $StopCompiling) -and $Arria)
{	$Library = "arriaii"
	$Files = @(
		"arriaii_atoms.vhd",
		"arriaii_components.vhd",
		"arriaii_hssi_components.vhd",
		"arriaii_hssi_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile arriaii_pcie_hip library
if ((-not $StopCompiling) -and $Arria)
{	$Library = "arriaii_pcie_hip"
	$Files = @(
		"arriaii_pcie_hip_components.vhd",
		"arriaii_pcie_hip_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile arriaiigz library
if ((-not $StopCompiling) -and $Arria)
{	$Library = "arriaiigz"
	$Files = @(
		"arriaiigz_atoms.vhd",
		"arriaiigz_components.vhd",
		"arriaiigz_hssi_components.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile arriav library
if ((-not $StopCompiling) -and $Arria)
{	$Library = "arriav"
	$Files = @(
		"arriav_atoms.vhd",
		"arriav_components.vhd",
		"arriav_hssi_components.vhd",
		"arriav_hssi_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile arriavgz library
if ((-not $StopCompiling) -and $Arria)
{	$Library = "arriavgz"
	$Files = @(
		"arriavgz_atoms.vhd",
		"arriavgz_components.vhd",
		"arriavgz_hssi_components.vhd",
		"arriavgz_hssi_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile arriavgz_pcie_hip library
if ((-not $StopCompiling) -and $Arria)
{	$Library = "arriavgz_pcie_hip"
	$Files = @(
		"arriavgz_pcie_hip_components.vhd",
		"arriavgz_pcie_hip_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile cycloneiv library
if ((-not $StopCompiling) -and $Cyclone)
{	$Library = "cycloneiv"
	$Files = @(
		"cycloneiv_atoms.vhd",
		"cycloneiv_components.vhd",
		"cycloneiv_hssi_components.vhd",
		"cycloneiv_hssi_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile cycloneiv_pcie_hip library
if ((-not $StopCompiling) -and $Cyclone)
{	$Library = "cycloneiv_pcie_hip"
	$Files = @(
		"cycloneiv_pcie_hip_components.vhd",
		"cycloneiv_pcie_hip_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile cycloneive library
if ((-not $StopCompiling) -and $Cyclone)
{	$Library = "cycloneive"
	$Files = @(
		"cycloneive_atoms.vhd",
		"cycloneive_components.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile cyclonev library
if ((-not $StopCompiling) -and $Cyclone)
{	$Library = "cyclonev"
	$Files = @(
		"cyclonev_atoms.vhd",
		"cyclonev_components.vhd",
		"cyclonev_hssi_components.vhd",
		"cyclonev_hssi_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile stratixiv library
if ((-not $StopCompiling) -and $Stratix)
{	$Library = "stratixiv"
	$Files = @(
		"stratixiv_atoms.vhd",
		"stratixiv_components.vhd",
		"stratixiv_hssi_components.vhd",
		"stratixiv_hssi_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile stratixiv_pcie_hip library
if ((-not $StopCompiling) -and $Stratix)
{	$Library = "stratixiv_pcie_hip"
	$Files = @(
		"stratixiv_pcie_hip_components.vhd",
		"stratixiv_pcie_hip_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile stratixv library
if ((-not $StopCompiling) -and $Stratix)
{	$Library = "stratixv"
	$Files = @(
		"stratixv_atoms.vhd",
		"stratixv_components.vhd",
		"stratixv_hssi_components.vhd",
		"stratixv_hssi_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile stratixv_pcie_hip library
if ((-not $StopCompiling) -and $Stratix)
{	$Library = "stratixv_pcie_hip"
	$Files = @(
		"stratixv_pcie_hip_components.vhd",
		"stratixv_pcie_hip_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile fiftyfivenm library
if ((-not $StopCompiling) -and $Nanometer)
{	$Library = "fiftyfivenm"
	$Files = @(
		"fiftyfivenm_atoms.vhd",
		"fiftyfivenm_components.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

# compile twentynm library
if ((-not $StopCompiling) -and $Nanometer)
{	$Library = "twentynm"
	$Files = @(
		"twentynm_atoms.vhd",
		"twentynm_components.vhd",
		"twentynm_hip_components.vhd",
		"twentynm_hip_atoms.vhd",
		"twentynm_hssi_components.vhd",
		"twentynm_hssi_atoms.vhd"
	)
	$SourceFiles = $Files | % { "$SourceDirectory\$_" }

	if (Test-Path $SourceFiles[0])
	{	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling Altera libraries " -NoNewline
if ($ErrorCount -gt 0)
{	Write-Host "[FAILED]" -ForegroundColor Red        }
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green  }

Exit-CompileScript
