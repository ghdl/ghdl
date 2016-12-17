Write-Host "Configuring GHDL for MinGW64, LLVM-3.5..."

$GHDL_BUILD_DIR =  "$($env:APPVEYOR_BUILD_FOLDER)\build\mingw64-llvm"
$GHDL_PREFIX_DIR = "C:\Tools\GHDL-0.34-dev-mingw64-llvm"

$env:GHDL_BUILD_DIR =  $GHDL_BUILD_DIR
$env:GHDL_PREFIX_DIR = $GHDL_PREFIX_DIR

mkdir $GHDL_BUILD_DIR | cd
c:\msys64\usr\bin\bash.exe -c "../../configure --prefix=$GHDL_PREFIX_DIR --with-llvm-config" 2>&1 | Restore-NativeCommandStream | %{ "$_" }

cd $env:APPVEYOR_BUILD_FOLDER
exit 0
