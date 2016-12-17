Write-Host "Building GHDL and libraries..."
cd "$($env:APPVEYOR_BUILD_FOLDER)\testsuite"

$env:GHDL="$($env:GHDL_PREFIX_DIR)\bin\ghdl.exe"

c:\msys64\usr\bin\bash.exe -c "./testsuite.sh" 2>&1 | Restore-NativeCommandStream | %{ "$_" }

cd $env:APPVEYOR_BUILD_FOLDER
exit 0
