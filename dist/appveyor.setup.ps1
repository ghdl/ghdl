Write-Host "Installing GHDL and libraries..."
cd $env:GHDL_BUILD_DIR
c:\msys64\usr\bin\make.exe install 2>&1 | Restore-NativeCommandStream | %{ "$_" }

cd $env:APPVEYOR_BUILD_FOLDER
exit 0
