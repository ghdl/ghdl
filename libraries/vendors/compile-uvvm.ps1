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
# Script to compile the UVVM libraries and verification models for GHDL on Windows.
#
# .DESCRIPTION
# This CmdLet:
#   (1) creates a subdirectory in the current working directory
#   (2) compiles all UVVM packages
#
[CmdletBinding()]
param(
	# Show the embedded help page(s).
	[switch]$Help =               $false,

	# Compile all packages.
	[switch]$All =                $false,

	# Compile all UVVM packages.
	[switch]$UVVM =               $false,
		# Compile all UVVM Utility packages.
		[switch]$UVVM_Utilities =           $false,
		# Compile all UVVM VCC Framework packages.
		[switch]$UVVM_VCC_Framework =       $false,
	# Compile all UVVM Verification IPs (VIPs).
	[switch]$UVVM_VIP =           $false,
	  # Compile VIP: Avalon_MM
	  [switch]$UVVM_VIP_Avalon_MM =       $false,
	  # Compile VIP: AXI-Lite
	  [switch]$UVVM_VIP_AXI_Lite =        $false,
	  # Compile VIP: AXI-Stream
	  [switch]$UVVM_VIP_AXI_Stream =      $false,
	  # Compile VIP: Clock Generator
	  [switch]$UVVM_VIP_Clock_Generator = $false,
	  # Compile VIP: GPIO
	  [switch]$UVVM_VIP_GPIO =            $false,
	  # Compile VIP: I2C
	  [switch]$UVVM_VIP_I2C =             $false,
	  # Compile VIP: Scoreboard
	  [switch]$UVVM_VIP_Scoreboard =      $false,
	  # Compile VIP: SBI (Simple Byte Interface)
	  [switch]$UVVM_VIP_SBI =             $false,
	  # Compile VIP: SPI
	  [switch]$UVVM_VIP_SPI =             $false,
	  # Compile VIP: UART
	  [switch]$UVVM_VIP_UART =            $false,

	# Clean up directory before analyzing.
	[switch]$Clean =              $false,

	#Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings =   $false,
	# Halt on errors.
	[switch]$HaltOnError =        $false,

	# Set vendor library source directory.
	[string]$Source =             "",
	# Set output directory name.
	[string]$Output =             "",
	# Set GHDL binary directory.
	[string]$GHDL =               ""
)

# ---------------------------------------------
# save working directory
$WorkingDir =     Get-Location

# set default values
$EnableDebug =    [bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
$EnableVerbose =  [bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug

# load modules from GHDL's 'vendors' library directory
$EnableVerbose -and  (Write-Host "Loading modules..." -ForegroundColor Gray  ) | Out-Null
$EnableDebug -and    (Write-Host "  Import-Module $PSScriptRoot\config.psm1 -Verbose:`$$false -Debug:`$$false -ArgumentList `"UVVM`"" -ForegroundColor DarkGray  ) | Out-Null
Import-Module $PSScriptRoot\config.psm1 -Verbose:$false -ArgumentList "UVVM"
$EnableDebug -and    (Write-Host "  Import-Module $PSScriptRoot\shared.psm1 -Verbose:`$$false -Debug:`$$false -ArgumentList @(`"UVVM`", `"$WorkingDir`")" -ForegroundColor DarkGray  ) | Out-Null
Import-Module $PSScriptRoot\shared.psm1 -Verbose:$false -ArgumentList @("UVVM", "$WorkingDir")

# Display help if no command was selected
if ($Help -or (-not ($All -or $Clean -or
                    ($UVVM -or      ($UVVM_Utilities -or $UVVM_VVC_Framework)) -or
                    ($UVVM_VIP -or  ($UVVM_VIP_Avalon_MM -or $UVVM_VIP_AXI_Lite -or $UVVM_VIP_AXI_Stream -or
                                     $UVVM_VIP_Clock_Generator -or $UVVM_VIP_GPIO -or $UVVM_VIP_I2C -or $UVVM_VIP_SBI -or
                                     $UVVM_VIP_Scoreboard -or $UVVM_VIP_SPI -or $UVVM_VIP_UART))
	)))
{	Get-Help $MYINVOCATION.MyCommand.Path -Detailed
	Exit-CompileScript
}

if ($All)
{	$UVVM =                     $true
	$UVVM_VIP =                 $true
}
if ($UVVM)
{	$UVVM_Utilities =           $true
	$UVVM_VCC_Framework =       $true
}
if ($UVVM_VIP)
{	$UVVM_VIP_Avalon_MM =       $true
	$UVVM_VIP_AXI_Lite =        $true
	$UVVM_VIP_AXI_Stream =      $true
	$UVVM_VIP_Clock_Generator = $true
	$UVVM_VIP_GPIO =            $true
	$UVVM_VIP_I2C =             $true
	$UVVM_VIP_Scoreboard =      $true
	$UVVM_VIP_SBI =             $true
	$UVVM_VIP_SPI =             $true
	$UVVM_VIP_UART =            $true
}


$SourceDirectory =      Get-SourceDirectory $Source ""
$DestinationDirectory = Get-DestinationDirectory $Output
$GHDLBinary =           Get-GHDLBinary $GHDL

# create "uvvm" directory and change to it
New-DestinationDirectory $DestinationDirectory
cd $DestinationDirectory


$VHDLVersion,$VHDLStandard,$VHDLFlavor = Get-VHDLVariables

# define global GHDL Options
$Analyze_Parameters = @(
	"--mb-comments",
	"-Wbinding",
	"-fexplicit",
	"-Wno-shared"          # UVVM specific
)
if (-not $EnableDebug)
{	$Analyze_Parameters += @(
		"-Wno-hide"
	)
}
if (-not ($EnableVerbose -or $EnableDebug))
{ $Analyze_Parameters += @(
		"-Wno-others",
		"-Wno-static"
	)
}
$Analyze_Parameters += @(
	"--ieee=$VHDLFlavor",
	"--no-vital-checks",
	"--std=$VHDLStandard",
	"-frelaxed",
	"-P$DestinationDirectory"
)


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

$UVVM_Util_Files = @(
	"uvvm_util\src\types_pkg.vhd",
	"uvvm_util\src\adaptations_pkg.vhd",
	"uvvm_util\src\string_methods_pkg.vhd",
	"uvvm_util\src\protected_types_pkg.vhd",
	"uvvm_util\src\global_signals_and_shared_variables_pkg.vhd",
	"uvvm_util\src\hierarchy_linked_list_pkg.vhd",
	"uvvm_util\src\alert_hierarchy_pkg.vhd",
	"uvvm_util\src\license_pkg.vhd",
	"uvvm_util\src\methods_pkg.vhd",
	"uvvm_util\src\bfm_common_pkg.vhd",
	"uvvm_util\src\uvvm_util_context.vhd"
)
$UVVM_VVC_Files = @(
	"uvvm_vvc_framework\src\ti_vvc_framework_support_pkg.vhd",
	"uvvm_vvc_framework\src\ti_generic_queue_pkg.vhd",
	"uvvm_vvc_framework\src\ti_data_queue_pkg.vhd",
	"uvvm_vvc_framework\src\ti_data_fifo_pkg.vhd",
	"uvvm_vvc_framework\src\ti_data_stack_pkg.vhd"
	"uvvm_vvc_framework\src\ti_uvvm_engine.vhd"
)


Write-Host "Reading VIP compile order files..." -ForegroundColor Cyan
$VIP_Files = @{}
foreach ($VIPDirectory in (Get-ChildItem -Path $SourceDirectory -Directory "*VIP*"))
{	$VIPName =      $VIPDirectory.Name
	$VIPVariable =  $VIPName.Substring(7).ToUpper().Replace("AXI", "AXI_")

	$EnableVerbose -and (Write-Host "  Found VIP: $VIPName"    -ForegroundColor Gray                                                                ) | Out-Null
	$EnableDebug -and   (Write-Host "    Reading compile order from '$SourceDirectory\$VIPName\script\compile_order.txt'" -ForegroundColor DarkGray ) | Out-Null

	$VIPFiles = @()
	$CompileOrder = Get-Content "$SourceDirectory\$VIPName\script\compile_order.txt"
	foreach ($Line in $CompileOrder)
	{	if ($Line.StartsWith("# "))
		{	if ($Line.StartsWith("# library "))
			{	$VIPName = $Line.Substring(10) }
			else
			{ Write-Host "Unknown parser instruction in compile order file." -ForegroundColor Yellow }
		}
		else
		{	$Path = Resolve-Path "$SourceDirectory\$VIPName\script\$Line"
			$VIPFiles += $Path
		}
	}

	if ($EnableDebug)
	{	Write-Host "    VHDL Library name: $VIPName"    -ForegroundColor DarkGray
		foreach ($File in $VIPFiles)
	  {	Write-Host "      $File" -ForegroundColor DarkGray }
	}

	$VIP_Files[$VIPName] = @{
	  "Variable" =  "UVVM_$VIPVariable";
	  "Library" =    $VIPName;
	  "Files" =      $VIPFiles
	}
}


# UVVM packages
# ==============================================================================
# compile uvvm_util library
if ((-not $StopCompiling) -and $UVVM_Utilities)
{	$Library = "uvvm_util"
	$SourceFiles = $UVVM_Util_Files | % { "$SourceDirectory\$_" }

	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}

# compile uvvm_vvc_framework library
if ((-not $StopCompiling) -and $UVVM_VCC_Framework)
{	$Library = "uvvm_vvc_framework"
	$SourceFiles = $UVVM_VVC_Files | % { "$SourceDirectory\$_" }

	$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
	$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
}


foreach ($vip in $VIP_Files.Keys)
{	if ((-not $StopCompiling) -and (Get-Variable $VIP_Files[$vip]["Variable"] -ValueOnly))
	{	$Library =      $VIP_Files[$vip]["Library"]
		$SourceFiles =  $VIP_Files[$vip]["Files"] #| % { "$SourceDirectory\$_" }

		$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling UVVM packages " -NoNewline
if ($ErrorCount -gt 0)
{	Write-Host "[FAILED]" -ForegroundColor Red        }
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green  }

Exit-CompileScript
