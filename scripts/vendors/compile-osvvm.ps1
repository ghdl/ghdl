# ==============================================================================
#  Authors:
#    Patrick Lehmann
#
# ==============================================================================
#   Copyright (C) 2017-2021 Patrick Lehmann - Boetzingen, Germany
#   Copyright (C) 2015-2016 Patrick Lehmann - Dresden, Germany
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
# Script to compile the OSVVM libraries and verification models for GHDL on Windows.
#
# .DESCRIPTION
# This CmdLet:
#   (1) creates a subdirectory in the current working directory
#   (2) compiles all OSVVM packages
#
[CmdletBinding()]
param(
	# Show the embedded help page(s)
	[switch]$Help =             $false,

	# Compile all libraries and packages.
	[switch]$All =              $false,

	# Compile all OSVVM packages (utility library and common packages).
	[switch]$OSVVM =            $false,
		# Compile all OSVVM 'utility' packages.
		[switch]$OSVVM_Utilities =          $false,
		# Compile all OSVVM 'common' packages.
		[switch]$OSVVM_Common =             $false,

	# Compile all OSVVM verfication IPs.
	[switch]$OSVVM_VIP =        $false,
		# Compile OSVVM's AXI4 models (AXI4, AXI4-Lite, AXI4-Stream).
		[switch]$OSVVM_VIP_AXI4 =           $false,
		# Compile OSVVM's UART model.
		[switch]$OSVVM_VIP_UART =           $false,

	# Clean up directory before analyzing.
	[switch]$Clean =            $false,

	# Skip warning messages. (Show errors only.)
	[switch]$SuppressWarnings = $false,
	# Halt on errors
	[switch]$HaltOnError =      $false,

	# Set vendor library source directory.
	[string]$Source =            "",
	# Set output directory name.
	[string]$Output =            "",
	# Set path to GHDL's executable, e.g. <MyGHDLPath>/bin/ghdl.exe
	[string]$GHDL =              ""
)

# ---------------------------------------------
# save working directory
$WorkingDir =     Get-Location

# set default values
$EnableDebug =    [bool]$PSCmdlet.MyInvocation.BoundParameters["Debug"]
$EnableVerbose =  [bool]$PSCmdlet.MyInvocation.BoundParameters["Verbose"] -or $EnableDebug

# load modules from GHDL's 'vendors' library directory
$EnableVerbose -and (Write-Host "Loading modules..." -ForegroundColor Gray  ) | Out-Null
$EnableDebug -and   (Write-Host "  Import-Module $PSScriptRoot\config.psm1 -Verbose:`$$false -Debug:`$$false -ArgumentList `"OSVVM`"" -ForegroundColor DarkGray                      ) | Out-Null
Import-Module $PSScriptRoot\config.psm1 -Verbose:$false -ArgumentList "OSVVM"
$EnableDebug -and   (Write-Host "  Import-Module $PSScriptRoot\shared.psm1 -Verbose:`$$false -Debug:`$$false -ArgumentList @(`"OSVVM`", `"$WorkingDir`")" -ForegroundColor DarkGray  ) | Out-Null
Import-Module $PSScriptRoot\shared.psm1 -Verbose:$false -ArgumentList @("OSVVM", "$WorkingDir")

# Display help if no command was selected
if ($Help -or (-not ($All -or $Clean -or
                    ($OSVVM -or     ($OSVVM_Utilities -or $OSVVM_Common)) -or
                    ($OSVVM_VIP -or ($OSVVM_VIP_AXI4 -or $OSVVM_VIP_UART))
	)))
{	Get-Help $MYINVOCATION.MyCommand.Path -Detailed
	Exit-CompileScript
}

if ($All)
{	$OSVVM =                    $true
	$OSVVM_VIP =                $true
}
if ($OSVVM)
{	$OSVVM_Utilities =          $true
	$OSVVM_Common =             $true
}
if ($OSVVM_VIP)
{	$OSVVM_VIP_AXI4 =           $true
	$OSVVM_VIP_UART =           $true
}


$SourceDirectory =      Get-SourceDirectory $Source ""
$DestinationDirectory = Get-DestinationDirectory $Output
$GHDLBinary =           Get-GHDLBinary $GHDL

# create "Altera" directory and change to it
New-DestinationDirectory $DestinationDirectory
cd $DestinationDirectory

$VHDLVersion,$VHDLStandard,$VHDLFlavor = Get-VHDLVariables -VHDL2008

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


function Get-CompileOrderedFiles
{	<#
		.SYNOPSIS
		Read *.pro files

		.DESCRIPTION
		Recursive function to read *.pro files

		.PARAMETER CurrentDirectory
		Current working directory. All paths in *.pro files are relative to this directory.
		.PARAMETER CompileOrderFile
		*.pro file to read and analyze
		.PARAMETER Level
		Level since root directory
	#>
	[CmdletBinding()]
	param(
		[string]$CurrentDirectory,
		[string]$CompileOrderFile,
		[int]$Level = 0
	)

#	Write-Host "$CurrentDirectory - $CompileOrderFile - $Level"

	$FileSets =  [ordered]@{}
	$Libraries = [ordered]@{}
	$Libraries["work"] = @{
		"Library" = "work";
		"Files"   = @()
	}
	$CoverageFile = ""

	$CompileOrder = Get-Content "$CurrentDirectory\$CompileOrderFile"
	foreach ($Line in $CompileOrder)
	{	if ($Line.StartsWith("#") -or $Line -eq "")
		{ continue }
		elseif ($Line.StartsWith("include "))
		{	$IncludeFile = $Line.Substring(8)
			$File = "$CurrentDirectory\$IncludeFile"
			if (Test-Path $File)
			{	$Dir = Split-Path -Path $File -Resolve
				$File = Split-Path -Path $File -Resolve -Leaf

				if ($Level -eq 0) # VIP Level
				{	$VIPName = Split-Path -Path $Dir -Leaf
					$Lib = Get-CompileOrderedFiles $Dir $File ($Level + 1)

					$VIPName = $VIPName.ToUpper()
					$VariableName = switch ( $VIPName )
						{	"OSVVM"  { "OSVVM_Utilities"    }
							"COMMON" { "OSVVM_Common"       }
							default  { "OSVVM_VIP_$VIPName" }
						}

					$FileSets[$VIPName] = @{
						"Variable" =  $VariableName;
						"Component" = $VIPName;
						"Libraries" = $Lib
					}
				}
				else
				{	$Lib = Get-CompileOrderedFiles $Dir $File ($Level + 1)
					foreach ($LibName in $Lib.Keys)
					{	if ($LibName -eq "work")
						{	$LibraryName = $Libraries["work"]["Library"]
							$Libraries[$LibraryName]["Files"] += $Lib["work"]["Files"]
						}
						elseif ($Libraries.Contains($LibName))
						{	$Libraries[$LibName]["Files"] += $Lib[$LibName]["Files"] }
						else
						{	$Libraries[$LibName] = @{
								"Library" = $LibName;
								"Files" =   $Lib[$LibName]["Files"]
							}
						}
					} # for LibName
				} # Level
			} # Test-Path
			continue
		} # include
		elseif ($Line.StartsWith("if"))
		{ continue }
		elseif ($Line.StartsWith("}"))
		{ continue }
		elseif ($Line.StartsWith("library "))
		{	$LibraryName = $Line.Substring(8)
			$Libraries["work"]["Library"] = $LibraryName
			$Libraries[$LibraryName] = @{
				"Library" = $LibraryName;
				"Files"   = @()
			}
			continue
		}
		elseif ($Line.StartsWith("analyze "))
		{	$SourceFile = $Line.Substring(8) }
		elseif ($Line.StartsWith("  analyze "))
		{	if ($CoverageFile -eq "")
			{	$CoverageFile = $Line.Substring(10)
				continue
			}
			else
			{	$SourceFile = $Line.Substring(10) }
		}
		else
		{ Write-Host "[ERROR]: Unknown instruction in compile order file." -ForegroundColor Red
			Write-Host "  $Line"
			continue
		}

		$Path = "$CurrentDirectory\$SourceFile"
		try
		{	$LibraryName = $Libraries["work"]["Library"]
			$Libraries[$LibraryName]["Files"] += Resolve-Path $Path  }
		catch
		{	Write-Host "[ERROR]: When resolving path '$Path'." -ForegroundColor Red }
	}

	if ($Level -eq 0)
	{	return $FileSets	}
	else
	{	return $Libraries }
}


$CompileOrderFile = "OsvvmLibraries.pro"
if (Test-Path "$SourceDirectory\$CompileOrderFile")
{	$FileSets = Get-CompileOrderedFiles $SourceDirectory $CompileOrderFile }
else
{	Write-Host "[ERROR]: File '$CompileOrderFile' not found." -ForegroundColor Red }


#	$CompileOrderFile = "osvvm.pro"
#	$EnableVerbose -and (Write-Host "  Search for 'osvvm' directory..." -ForegroundColor Gray                          ) | Out-Null
#	if (Test-Path "$SourceDirectory\$CompileOrderFile")
#	{	$PackageDirectory = $SourceDirectory	       }
#	elseif (Test-Path "$SourceDirectory\osvvm\$CompileOrderFile")
#	{	$PackageDirectory = "$SourceDirectory\osvvm" }
#	$EnableDebug -and   (Write-Host "    Found '$CompileOrderFile' in '$PackageDirectory'" -ForegroundColor DarkGray  ) | Out-Null

# Analyze OSVVM library and models
# ==============================================================================
foreach ($VIPName in $FileSets.Keys)
{	$VariableName = $FileSets[$VIPName]["Variable"]
	try
	{	$Enabled = Get-Variable $VariableName -ValueOnly }
	catch
	{	Write-Host "[ERROR]: Found a new OSVVM component not supported by this script. Skipping." -ForegroundColor Red
		continue
	}

	if ((-not $StopCompiling) -and (Get-Variable $VariableName -ValueOnly))
	{	Write-Host ("Component: " + $FileSets[$VIPName]["Component"]) -ForegroundColor Magenta

		foreach ($LibraryName in $FileSets[$VIPName]["Libraries"].Keys)
		{	if ($LibraryName -eq "work")
			{	if ($FileSets[$VIPName]["Libraries"][$LibraryName]["Files"].Count -ne 0)
				{	Write-Host ("[ERROR]: Library 'works' contains " + $FileSets[$VIPName]["Libraries"][$LibraryName]["Files"].Count + " files.") -ForegroundColor Red
					foreach ($File in $FileSets[$VIPName]["Libraries"][$LibraryName]["Files"])
					{	Write-Host "  $File" -ForegroundColor Red }
				}
				continue
			}

			$Library =      $FileSets[$VIPName]["Libraries"][$LibraryName]["Library"]
			$SourceFiles =  $FileSets[$VIPName]["Libraries"][$LibraryName]["Files"]

			$ErrorCount += Start-PackageCompilation $GHDLBinary $Analyze_Parameters $DestinationDirectory $Library $VHDLVersion $SourceFiles $SuppressWarnings $HaltOnError -Verbose:$EnableVerbose -Debug:$EnableDebug
			$StopCompiling = $HaltOnError -and ($ErrorCount -ne 0)
		}
	}
}

Write-Host "--------------------------------------------------------------------------------"
Write-Host "Compiling OSVVM " -NoNewline
if ($ErrorCount -gt 0)
{	Write-Host "[FAILED]" -ForegroundColor Red        }
else
{	Write-Host "[SUCCESSFUL]" -ForegroundColor Green  }

Exit-CompileScript
