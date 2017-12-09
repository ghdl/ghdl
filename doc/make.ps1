# .SYNOPSIS
# Please use '.\make.ps1 -<target>' where <target> is one of:
#   html, dirhtml, singlehtml, pickle, json, linkcheck
#
# .DESCRIPTION
# This is a front-end script for Sphinx.
#
# .EXAMPLE
#  .\make.ps1 -clean
# Cleanup the build directory.
# .EXAMPLE
#  .\make.ps1 -html
# Build documentation as HTML pages.
# .EXAMPLE
#  .\make.ps1 -clean -html -linkcheck
# Combine multiple commands in a single call
#
#
# ==========================================================================
# Copyright � 2016-2017 Patrick Lehmann - Dresden, Germany
# ==========================================================================
[CmdletBinding()]
param(
  # Make all targets
  [switch]$all =        $false,
  # Extract VHDL documentation
  [switch]$html =       $false,
  # Make HTML files named index.html in directories
  [switch]$dirhtml =    $false,
  # Make a single large HTML file
  [switch]$singlehtml = $false,
  # Make a PDF file
  [switch]$latex =			$false,
  [switch]$pdf =				$false,
  # Make pickle files
  [switch]$pickle =     $false,
  # Make json files
  [switch]$json =       $false,
  # Check all external links for integrity
  [switch]$linkcheck =  $false,
  # Clean up directory before running Sphinx.
  [switch]$clean =      $false,
  # Show the embedded help page(s).
  [switch]$help =       $false
)

# resolve paths
$WorkingDir =     Get-Location
$SphinxRootDir =  Convert-Path (Resolve-Path ($PSScriptRoot))

# set default values
$EnableVerbose =  $PSCmdlet.MyInvocation.BoundParameters["Verbose"]
$EnableDebug =    $PSCmdlet.MyInvocation.BoundParameters["Debug"]
if ($EnableVerbose -eq $null)  { $EnableVerbose =  $false }
if ($EnableDebug   -eq $null)  { $EnableDebug =    $false }
if ($EnableDebug   -eq $true)  { $EnableVerbose =  $true  }

# Display help if no command was selected
$Help = $Help -or (-not ($all -or $html -or $dirhtml -or $singlehtml -or $latex -or $pdf -or $json -or $pickle -or $linkcheck -or $clean -or $help))

function Exit-Script
{ <#
    .PARAMETER ExitCode
    ExitCode of this script run
  #>
  [CmdletBinding()]
  param([int]$ExitCode = 0)
  
  # restore environment
  # rm env:GHDL -ErrorAction SilentlyContinue
  
  cd $WorkingDir
  
  # unload modules
  # Remove-Module precompile -Verbose:$false
  
	Pop-EnvironmentBlock
  # exit with exit code
  exit $ExitCode
}

if ($Help)
{ Get-Help $MYINVOCATION.InvocationName -Detailed
  Exit-Script
}

Push-EnvironmentBlock
#$env:Path += ";C:\Tools\Graphviz\2.38\bin"

if ($All)
{ $clean =      $true
  $html =       $true
  $dirhtml =    $true
  $singlehtml = $true
}

$SphinxBuild =    if (-not $env:SPHINXBUILD) { "sphinx-build" } else { $env:SPHINXBUILD }
# $BuildDir =       "$SphinxRootDir\{{ rbuilddir }}"
# $SourceDir =      "$SphinxRootDir\{{ rsrcdir }}"
$BuildDir =       "$SphinxRootDir\_build"   # for local testing, can be removed in the future
$SourceDir =      "$SphinxRootDir\."        # for local testing, can be removed in the future
$AllSphinxOpts =  "-d $BuildDir\doctrees ${env:SPHINXOPTS} $SourceDir"
$I18NSphinxOpts = "${env:SPHINXOPTS} $SourceDir"
# ------------------------------------------------------------------------------
# TODO: add paper options
# ------------------------------------------------------------------------------
# if NOT "%PAPER%" == "" (
#   set ALLSPHINXOPTS=-D latex_paper_size=%PAPER% %ALLSPHINXOPTS%
#   set I18NSPHINXOPTS=-D latex_paper_size=%PAPER% %I18NSPHINXOPTS%
# )

# Check if sphinx-build is available and fallback to Python version if any
# ------------------------------------------------------------------------------
# TODO: add testings if sphinxbuild can be called and/or is installed
# ------------------------------------------------------------------------------
#  %SPHINXBUILD% 1>NUL 2>NUL
#  if errorlevel 9009 goto sphinx_python
#  goto sphinx_ok
#
#  :sphinx_python
#
#  set SPHINXBUILD=python -m sphinx.__init__
#  %SPHINXBUILD% 2> nul
#  if errorlevel 9009 (
#    echo.
#    echo.The 'sphinx-build' command was not found. Make sure you have Sphinx
#    echo.installed, then set the SPHINXBUILD environment variable to point
#    echo.to the full path of the 'sphinx-build' executable. Alternatively you
#    echo.may add the Sphinx directory to PATH.
#    echo.
#    echo.If you don't have Sphinx installed, grab it from
#    echo.http://sphinx-doc.org/
#    exit /b 1
#  )


if ($clean)
{ $EnableVerbose -and (Write-Host "Cleaning build directory '$BuildDir'..." -Foreground DarkCyan  ) | Out-Null
	$EnableDebug   -and (Write-Host "  dir -Path $BuildDir * -Directory | rmdir -Recurse"           ) | Out-Null
	dir -Path $BuildDir * -Directory | rmdir -Recurse
	
  Write-Host "Cleaning finished." -Foreground Green
}

if ($html)
{ $expr = "$SphinxBuild -b html -t GHDLInternal $AllSphinxOpts $BuildDir\html"
  $EnableVerbose -and (Write-Host "Building target 'html' into '$BuildDir\html'..." -Foreground DarkCyan  ) | Out-Null
	$EnableDebug   -and (Write-Host "  $expr" -Foreground Cyan ) | Out-Null
	Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  Write-Host "Build finished. The HTML pages are in $BuildDir\html." -Foreground Green
}

if ($dirhtml)
{ $expr = "$SphinxBuild -b dirhtml $AllSphinxOpts $BuildDir\dirhtml"
  $EnableVerbose -and (Write-Host "Building target 'dirhtml' into '$BuildDir\dirhtml'..." -Foreground DarkCyan  ) | Out-Null
	$EnableDebug   -and (Write-Host "  $expr" -Foreground Cyan ) | Out-Null
  Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  Write-Host "Build finished. The HTML pages are in $BuildDir\dirhtml." -Foreground Green
}

if ($singlehtml)
{ $expr = "$SphinxBuild -b singlehtml $AllSphinxOpts $BuildDir\singlehtml"
  $EnableVerbose -and (Write-Host "Building target 'singlehtml' into '$BuildDir\singlehtml'..." -Foreground DarkCyan  ) | Out-Null
	$EnableDebug   -and (Write-Host "  $expr" -Foreground Cyan ) | Out-Null
  Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  Write-Host "Build finished. The HTML file is in $BuildDir\singlehtml." -Foreground Green
}

if ($latex)
{ $expr = "$SphinxBuild -b latex $AllSphinxOpts $BuildDir\pdf"
  $EnableVerbose -and (Write-Host "Building target 'latex' into '$BuildDir\pdf'..." -Foreground DarkCyan  ) | Out-Null
	$EnableDebug   -and (Write-Host "  $expr" -Foreground Cyan ) | Out-Null
  Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  Write-Host "Build finished. The LaTeX sources are in $BuildDir\pdf." -Foreground Green
}
if ($pdf)
{	cd "$BuildDir\pdf"

	cp "$BuildDir\pdf\GHDL.tex" "$BuildDir\pdf\GHDL.tex"
	
	$expr = "pdflatex.exe $BuildDir\pdf\GHDL.tex"
	$EnableVerbose -and (Write-Host "Building target 'pdf' into '$BuildDir\pdf'..." -Foreground DarkCyan  ) | Out-Null
	Write-Host "Compiling with pdflatex.exe..." -Foreground Yellow
	$EnableDebug   -and (Write-Host "  $expr" -Foreground Cyan ) | Out-Null
  Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  
	$expr = "makeindex.exe .\GHDL.idx"
	Write-Host "Creating index with makeindex.exe..." -Foreground Yellow
	$EnableDebug   -and (Write-Host "  $expr" -Foreground Cyan ) | Out-Null
  Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  
	$expr = "pdflatex.exe $BuildDir\pdf\GHDL.tex"
	Write-Host "Compiling with pdflatex.exe..." -Foreground Yellow
  Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  Write-Host "Build finished. The PDF file is in $BuildDir\pdf." -Foreground Green
}

if ($pickle)
{ $expr = "$SphinxBuild -b pickle $AllSphinxOpts $BuildDir\pickle"
  $EnableVerbose -and (Write-Host "Building target 'pickle' into '$BuildDir\pickle'..." -Foreground DarkCyan  ) | Out-Null
	$EnableDebug   -and (Write-Host "  $expr" -Foreground Cyan ) | Out-Null
  Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  Write-Host "Build finished. Now you can process the pickle files." -Foreground Green
}

if ($json)
{ $expr = "$SphinxBuild -b json $AllSphinxOpts $BuildDir\json"
  $EnableVerbose -and (Write-Host "Building target 'json' into '$BuildDir\json'..." -Foreground DarkCyan  ) | Out-Null
	$EnableDebug   -and (Write-Host "  $expr" -Foreground Cyan ) | Out-Null
  Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  Write-Host "Build finished. Now you can process the json files." -Foreground Green
}

if ($linkcheck)
{ $expr = "$SphinxBuild -b linkcheck $AllSphinxOpts $BuildDir\linkcheck"
  $EnableVerbose -and (Write-Host "Building target 'html' into '$BuildDir\html'..." -Foreground DarkCyan  ) | Out-Null
	$EnableDebug   -and (Write-Host "  $expr" -Foreground Cyan ) | Out-Null
  Invoke-Expression $expr
  if ($LastExitCode -ne 0)
  { Exit-Script 1 }
  Write-Host "Link check complete. Look for any errors in the above output or in $BuildDir\linkcheck\output.txt." -Foreground Green
}

Exit-Script
