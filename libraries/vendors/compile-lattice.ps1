# EMACS settings: -*-	tab-width: 2; indent-tabs-mode: t -*-
# vim: tabstop=2:shiftwidth=2:noexpandtab
# kate: tab-width 2; replace-tabs off; indent-width 2;
# 
# ==============================================================================
#	Authors:						Patrick Lehmann
# 
#	PowerShell Script:	Script to compile the simulation libraries from Lattice
#											Diamond for GHDL on Windows
# 
# Description:
# ------------------------------------
#	This is a PowerShell script (executable) which:
#		- creates a subdirectory in the current working directory
#		- compiles all Lattice Diamond simulation libraries and packages
#
# ==============================================================================
#	Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
#	Copyright (C) 2017 Patrick Lehmann - Freiburg, Germany
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
# This CmdLet compiles the simulation libraries from Lattice.
# 
# .DESCRIPTION
# This CmdLet:
#   (1) creates a subdirectory in the current working directory
#   (2) compiles all Lattice Diamond simulation libraries and packages
#       o Lattice device libraries:
#         - EC, ECP, ECP2, ECP3, ECP5U
#         - LPTM, LPTM2
#         - MachXO, MachXO2, MachXO3L
#         - SC, SCM
#         - XP, XP2
# 
[CmdletBinding()]
param(
	# Show the embedded help page(s)
	[switch]$Help =				$false,
	
	# Compile all libraries and packages.
	[switch]$All =				$false,
	
	# Compile the Lattice EC device libraries
	[switch]$ec =					$false,
	# Compile the Lattice ECP device libraries
	[switch]$ecp =				$false,
	# Compile the Lattice ECP2 device libraries
	[switch]$ecp2 =				$false,
	# Compile the Lattice ECP3 device libraries
	[switch]$ecp3 =				$false,
	# Compile the Lattice ECP5U device libraries
	[switch]$ecp5u =			$false,
	
	# Compile the Lattice LPTM device libraries
	[switch]$lptm =				$false,
	# Compile the Lattice LPTM2 device libraries
	[switch]$lptm2 =			$false,
	
	# Compile the Lattice MachXO device libraries
	[switch]$MachXO =			$false,
	# Compile the Lattice MachXO2 device libraries
	[switch]$MachXO2 =		$false,
	# Compile the Lattice MachXO3L device libraries
	[switch]$MachXO3L =		$false,
	
	# Compile the Lattice SC device libraries
	[switch]$sc =					$false,
	# Compile the Lattice SCM device libraries
	[switch]$scm =				$false,
	
	# Compile the Lattice XP device libraries
	[switch]$xp =					$false,
	# Compile the Lattice XP2 device libraries
	[switch]$xp2 =				$false,
	
	# Clean up directory before analyzing.
	[switch]$Clean =			$false,
	
	# Set VHDL Standard to '93
	[switch]$VHDL93 =			$false,
	# Set VHDL Standard to '08
	[switch]$VHDL2008 =		$false,
	
	# Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false,
	# Halt on errors.
	[switch]$HaltOnError =			$false,
	
	# Set vendor library source directory.
	[string]$Source =			"",
	# Set output directory name.
	[string]$Output =			"",
	# Set GHDL binary directory.
	[string]$GHDL =				""
)

# ---------------------------------------------
# save working directory
$WorkingDir =		Get-Location

# set default values
$EnableDebug =		[bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
$EnableVerbose =	[bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug

# load modules from GHDL's 'vendors' library directory
Import-Module $PSScriptRoot\config.psm1 -Verbose:$false -Debug:$false -ArgumentList "LatticeDiamond"
Import-Module $PSScriptRoot\shared.psm1 -Verbose:$false -Debug:$false -ArgumentList @("Lattice Diamond", "$WorkingDir")

# Display help if no command was selected
$Help = $Help -or (-not ($All -or 
										($ec -or $ecp -or $ecp2 -or $ecp3 -or $ecp5u) -or 
										($lptm -or $lptm2) -or 
										($MachXO -or $MachXO2 -or $MachXO3L) -or 
										($sc -or $scm) -or
										($xp -or $xp2) -or
										$Clean))

if ($Help)
{	Get-Help $MYINVOCATION.InvocationName -Detailed
	Exit-CompileScript
}
if ($All)
{	$ec =				$true
	$ecp =			$true
	$ecp2 =			$true
	$ecp3 =			$true
	$ecp5u =		$true
	$lptm =			$true
	$lptm2 =		$true
	$MachXO =		$true
	$MachXO2 =	$true
	$MachXO3L =	$true
	$sc =				$true
	$scm =			$true
	$xp =				$true
	$xp2 =			$true
}

function Get-LatticeDiamondDirectory
{	if (Test-Path env:FOUNDRY)
	{	return $FOUNDRY + "\..\" + (Get-VendorToolSourceDirectory)		}
	else
	{	$EnvSourceDir = ""
		foreach ($Drive in Get-PSDrive -PSProvider 'FileSystem')
		{	$Path = $Drive.Name + ":\" + "Lattice\Diamond"
			if (Test-Path $Path -PathType Container)
			{	foreach ($Major in 4..3)
				{	foreach ($Minor in 9..0)
					{	$Dir = $Path + "\" + $Major + "." + $Minor + "_x64"
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
				
$SourceDirectory =			Get-SourceDirectory $Source (Get-LatticeDiamondDirectory)
$DestinationDirectory =	Get-DestinationDirectory $Output
$GHDLBinary =						Get-GHDLBinary $GHDL

# create "Lattice" directory and change to it
New-DestinationDirectory $DestinationDirectory
cd $DestinationDirectory

$VHDLVersion,$VHDLStandard,$VHDLFlavor = Get-VHDLVariables $VHDL93 $VHDL2008

# define global GHDL Options
$GHDLOptions = @("-a", "-fexplicit", "-frelaxed-rules", "--mb-comments", "--warn-binding", "--ieee=$VHDLFlavor", "--no-vital-checks", "--std=$VHDLStandard", "-P$DestinationDirectory")

$StopCompiling =	$false
$ErrorCount =			0

$FileLists = @{
	"ec" =				@("ORCA_CMB.vhd", "ORCA_SEQ.vhd", "ORCACOMP.vhd", "ORCA_LUT.vhd", "ORCA_MISC.vhd", "ORCA_CNT.vhd", "ORCA_IO.vhd", "ORCA_MEM.vhd");
	"ecp" =				@("ORCA_CMB.vhd", "ORCA_SEQ.vhd", "ORCACOMP.vhd", "ORCA_LUT.vhd", "ORCA_MISC.vhd", "ORCA_CNT.vhd", "ORCA_IO.vhd", "ORCA_MEM.vhd");
	"ecp2" =			@("ECP2_CMB.vhd", "ECP2_SEQ.vhd", "ECP2COMP.vhd", "ECP2_CNT.vhd", "ECP2_IO.vhd", "ECP2_LUT.vhd", "ECP2_MEM.vhd", "ECP2_MISC.vhd", "ECP2_MULT.vhd", "ECP2_SL.vhd");
	"ecp3" =			@("ECP3_CMB.vhd", "ECP3_SEQ.vhd", "ECP3COMP.vhd", "ECP3_CNT.vhd", "ECP3_IO.vhd", "ECP3_LUT.vhd", "ECP3_MEM.vhd", "ECP3_MISC.vhd", "ECP3_MULT.vhd", "ECP3_SL.vhd");
	"ecp5u" =			@("ECP5U_CMB.vhd", "ECP5U_SEQ.vhd", "ECP5UCOMP.vhd", "ECP5U_IO.vhd", "ECP5U_LUT.vhd", "ECP5U_MEM.vhd", "ECP5U_MISC.vhd", "ECP5U_SL.vhd", "gsr_pur_assign.vhd");
	"lptm" =			@("MACHXO_CMB.vhd", "MACHXO_SEQ.vhd", "MACHXOCOMP.vhd", "MACHXO_CNT.vhd", "MACHXO_IO.vhd", "MACHXO_LUT.vhd", "MACHXO_MEM.vhd", "MACHXO_MISC.vhd");
	"lptm2" =			@("MACHXO2_CMB.vhd", "MACHXO2_SEQ.vhd", "MACHXO2COMP.vhd", "gsr_pur_assign.vhd", "MACHXO2_CNT.vhd", "MACHXO2_IO.vhd", "MACHXO2_LUT.vhd", "MACHXO2_MEM.vhd", "MACHXO2_MISC.vhd");
	"machxo" =		@("MACHXO_CMB.vhd", "MACHXO_SEQ.vhd", "MACHXOCOMP.vhd", "MACHXO_CNT.vhd", "MACHXO_IO.vhd", "MACHXO_LUT.vhd", "MACHXO_MEM.vhd", "MACHXO_MISC.vhd");
	"machxo2" =		@("MACHXO2_CMB.vhd", "MACHXO2_SEQ.vhd", "MACHXO2COMP.vhd", "MACHXO2_CNT.vhd", "gsr_pur_assign.vhd", "MACHXO2_IO.vhd", "MACHXO2_LUT.vhd", "MACHXO2_MEM.vhd", "MACHXO2_MISC.vhd");
	"machxo3l" =	@("MACHXO3L_CMB.vhd", "MACHXO3L_SEQ.vhd", "MACHXO3LCOMP.vhd", "gsr_pur_assign.vhd", "MACHXO3L_CNT.vhd", "MACHXO3L_IO.vhd", "MACHXO3L_LUT.vhd", "MACHXO3L_MEM.vhd", "MACHXO3L_MISC.vhd");
	"sc" =				@("ORCA_CMB.vhd", "ORCA_SEQ.vhd", "ORCACOMP.vhd", "ORCA_CNT.vhd", "ORCA_IO.vhd", "ORCA_MEM.vhd", "ORCA_MIS.vhd", "ORCA_SL.vhd");
	"scm" =				@("ORCA_CMB.vhd", "ORCA_SEQ.vhd", "ORCACOMP.vhd", "ORCA_CNT.vhd", "ORCA_IO.vhd", "ORCA_MEM.vhd", "ORCA_MIS.vhd", "ORCA_SL.vhd");
	"xp" =				@("ORCA_CMB.vhd", "ORCA_SEQ.vhd", "ORCACOMP.vhd", "ORCA_LUT.vhd", "ORCA_MISC.vhd", "ORCA_CNT.vhd", "ORCA_IO.vhd", "ORCA_MEM.vhd");
	"xp2" =				@("XP2_CMB.vhd", "XP2_SEQ.vhd", "XP2COMP.vhd", "XP2_CNT.vhd", "XP2_IO.vhd", "XP2_LUT.vhd", "XP2_MEM.vhd", "XP2_MISC.vhd", "XP2_MULT.vhd", "XP2_SL.vhd")
}

# Cleanup directories
# ==============================================================================
if ($Clean)
{	Write-Host "[ERROR]: '-Clean' is not implemented!" -ForegroundColor Red
	Exit-CompileScript -1
	
	Write-Host "Cleaning up vendor directory ..." -ForegroundColor Yellow
	rm *.cf
}


# Lattice EC library
# ==============================================================================
if ((-not $StopCompiling) -and $ec)
{	$Library = "ec"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice ECP library
# ==============================================================================
if ((-not $StopCompiling) -and $ecp)
{	$Library = "ecp"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice ECP2 library
# ==============================================================================
if ((-not $StopCompiling) -and $ecp2)
{	$Library = "ecp2"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice ECP3 library
# ==============================================================================
if ((-not $StopCompiling) -and $ecp3)
{	$Library = "ecp3"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice ECP5U library
# ==============================================================================
if ((-not $StopCompiling) -and $ecp5u)
{	$Library = "ecp5u"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice LPTM library
# ==============================================================================
if ((-not $StopCompiling) -and $lptm)
{	$Library = "lptm"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice LPTM2 library
# ==============================================================================
if ((-not $StopCompiling) -and $lptm2)
{	$Library = "lptm2"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice MachXO library
# ==============================================================================
if ((-not $StopCompiling) -and $MachXO)
{	$Library = "MachXO"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice MachXO2 library
# ==============================================================================
if ((-not $StopCompiling) -and $MachXO2)
{	$Library = "MachXO2"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice MachXO3L library
# ==============================================================================
if ((-not $StopCompiling) -and $machxo3l)
{	$Library = "machxo3l"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice SC library
# ==============================================================================
if ((-not $StopCompiling) -and $sc)
{	$Library = "sc"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice SCM library
# ==============================================================================
if ((-not $StopCompiling) -and $scm)
{	$Library = "scm"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice XP library
# ==============================================================================
if ((-not $StopCompiling) -and $xp)
{	$Library = "xp"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# Lattice XP2 library
# ==============================================================================
if ((-not $StopCompiling) -and $xp2)
{	$Library = "xp2"
	$SourceFiles = $FileLists[$Library] | % { "$SourceDirectory\$Library\src\$_" }
	
	$ErrorCount += 0
	Start-PackageCompilation $GHDLBinary $GHDLOptions $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling Lattice libraries " -NoNewline
if ($ErrorCount -gt 0)
{	Write-Host "[FAILED]" -ForegroundColor Red				}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green	}

Exit-CompileScript
