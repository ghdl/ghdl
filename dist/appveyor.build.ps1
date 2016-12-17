Write-Host "Building GHDL and libraries..."
cd $env:GHDL_BUILD_DIR
c:\msys64\usr\bin\make.exe 2>&1 | Restore-NativeCommandStream | %{ "$_" }

cd $env:APPVEYOR_BUILD_FOLDER
exit 0
