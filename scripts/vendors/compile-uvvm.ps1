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
		# Compile all UVVM VVC Framework packages.
		[switch]$UVVM_VVC_Framework =       $false,
	# Compile all UVVM Verification IPs (VIPs).
	[switch]$UVVM_VIP =           $false,
	  # Compile VIP: Avalon Memory Mapped
	  [switch]$UVVM_VIP_Avalon_MM =       $false,
	  # Compile VIP: Avalon Stream
	  [switch]$UVVM_VIP_Avalon_ST =       $false,
	  # Compile VIP: AXI
	  [switch]$UVVM_VIP_AXI =             $false,
	  # Compile VIP: AXI-Lite
	  [switch]$UVVM_VIP_AXI_Lite =        $false,
	  # Compile VIP: AXI-Stream
	  [switch]$UVVM_VIP_AXI_Stream =      $false,
	  # Compile VIP: Clock Generator
	  [switch]$UVVM_VIP_Clock_Generator = $false,
	  # Compile VIP: Error Injection
	  [switch]$UVVM_VIP_Error_Injection = $false,
	  # Compile VIP: Ethernet
	  [switch]$UVVM_VIP_Ethernet =        $false,
	  # Compile VIP: GMII
	  [switch]$UVVM_VIP_GMII =            $false,
	  # Compile VIP: GPIO
	  [switch]$UVVM_VIP_GPIO =            $false,
	  # Compile VIP: HVVC to VVC Bridge
	  [switch]$UVVM_VIP_HVVC2VVC =        $false,
	  # Compile VIP: I2C
	  [switch]$UVVM_VIP_I2C =             $false,
	  # Compile VIP: RGMII
	  [switch]$UVVM_VIP_RGMII =           $false,
	  # Compile VIP: SBI (Simple Byte Interface)
	  [switch]$UVVM_VIP_SBI =             $false,
	  # Compile VIP: Scoreboard
	  [switch]$UVVM_VIP_Scoreboard =      $false,
	  # Compile VIP: Specifaction Coverage
	  [switch]$UVVM_VIP_Spec_Cov =        $false,
	  # Compile VIP: SPI
	  [switch]$UVVM_VIP_SPI =             $false,
	  # Compile VIP: UART
	  [switch]$UVVM_VIP_UART =            $false,
	  # Compile VIP: Wishbone
	  [switch]$UVVM_VIP_WISHBONE =        $false,

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
	# Set path to GHDL's executable, e.g. <MyGHDLPath>/bin/ghdl.exe
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
                    ($UVVM_VIP -or  ($UVVM_VIP_Avalon_MM -or $UVVM_VIP_Avalon_ST -or $UVVM_VIP_AXI -or $UVVM_VIP_AXI_Lite -or
                                     $UVVM_VIP_AXI_Stream -or $UVVM_VIP_Clock_Generator -or $UVVM_VIP_Error_Injection -or
											               $UVVM_VIP_Ethernet -or $UVVM_VIP_GMII -or $UVVM_VIP_GPIO -or $UVVM_VIP_HVVC2VVC -or
											               $UVVM_VIP_I2C -or $UVVM_VIP_RGMII -or $UVVM_VIP_SBI -or $UVVM_VIP_Scoreboard -or
											               $UVVM_VIP_Spec_Cov -or $UVVM_VIP_SPI -or $UVVM_VIP_UART -or $UVVM_VIP_WISHBONE))
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
	$UVVM_VVC_Framework =       $true
}
if ($UVVM_VIP)
{	$UVVM_VIP_Avalon_MM =       $true
	$UVVM_VIP_Avalon_ST =       $true
	$UVVM_VIP_AXI =             $true
	$UVVM_VIP_AXI_Lite =        $true
	$UVVM_VIP_AXI_Stream =      $true
	$UVVM_VIP_Clock_Generator = $true
	$UVVM_VIP_Error_Injection = $true
	$UVVM_VIP_Ethernet =        $true
	$UVVM_VIP_GMII =            $true
	$UVVM_VIP_GPIO =            $true
	$UVVM_VIP_HVVC2VVC =        $true
	$UVVM_VIP_I2C =             $true
	$UVVM_VIP_RGMII =           $true
	$UVVM_VIP_SBI =             $true
	$UVVM_VIP_Scoreboard =      $true
	$UVVM_VIP_Spec_Cov =        $true
	$UVVM_VIP_SPI =             $true
	$UVVM_VIP_UART =            $true
	$UVVM_VIP_WISHBONE =        $true
}


$SourceDirectory =      Get-SourceDirectory $Source ""
$DestinationDirectory = Get-DestinationDirectory $Output
$GHDLBinary =           Get-GHDLBinary $GHDL

# create "uvvm" directory and change to it
New-DestinationDirectory $DestinationDirectory
cd $DestinationDirectory


$VHDLVersion,$VHDLStandard,$VHDLFlavor = Get-VHDLVariables -VHDL2008

# define global GHDL Options
$Analyze_Parameters = @(
	"--std=$VHDLStandard",
	"-fexplicit",
	"-frelaxed-rules",
	"--mb-comments",
	"-Wbinding",
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

Write-Host "Reading VIP compile order files..." -ForegroundColor Cyan
$VIP_Files = [ordered]@{}
foreach ($VIPName in (Get-Content "$SourceDirectory\script\component_list.txt"))
{	if ($VIPName.StartsWith("uvvm"))
	{ $VIPVariable = $VIPName.Substring(5).ToUpper()
		$VIPVariable = $VIPVariable.Replace("UTIL", "Utilities")
	}
	elseif ($VIPName.StartsWith("bitvis"))
	{	$VIPVariable = $VIPName.Substring(7).ToUpper()
		$VIPVariable = $VIPVariable.Replace("AXILITE", "AXI_LITE")
		$VIPVariable = $VIPVariable.Replace("AXISTREAM", "AXI_STREAM")
		$VIPVariable = $VIPVariable.Replace("HVVC_TO_VVC_BRIDGE", "HVVC2VVC")
	}
	$VIPVariable = "UVVM_$VIPVariable"

	$EnableVerbose -and (Write-Host "  Found VIP: $VIPName"    -ForegroundColor Gray                                                                ) | Out-Null
	$EnableDebug -and   (Write-Host "    Reading compile order from '$SourceDirectory\$VIPName\script\compile_order.txt'" -ForegroundColor DarkGray ) | Out-Null

	$VIPFiles = @()
	$CompileOrder = Get-Content "$SourceDirectory\$VIPName\script\compile_order.txt"
	foreach ($Line in $CompileOrder)
	{	$Line = $Line.Trim()
		if ($Line -eq "")
		{	continue }
		elseif ($Line.StartsWith("#"))
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
	  "Variable" =  $VIPVariable;
	  "Library" =   $VIPName;
	  "Files" =     $VIPFiles
	}
}


# UVVM packages
# ==============================================================================
foreach ($vip in $VIP_Files.Keys)
{	if ((-not $StopCompiling) -and (Get-Variable $VIP_Files[$vip]["Variable"] -ValueOnly))
	{	$Library =      $VIP_Files[$vip]["Library"]
		$SourceFiles =  $VIP_Files[$vip]["Files"]

		$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
		$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
	}
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling UVVM packages " -NoNewline
if ($ErrorCount -gt 0)
{	Write-Host "[FAILED]" -ForegroundColor Red
	Exit-CompileScript 1
}
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green
	Exit-CompileScript
}
