Write-Host "--------------------"
c:\msys64\mingw64\bin\gcc.exe -print-search-dirs
Write-Host "--------------------"
c:\msys64\mingw64\bin\clang.exe -print-search-dirs
Write-Host "--------------------"
c:\msys64\mingw64\bin\gcc.exe -v 2>&1 | %{ "$_" }
# c:\msys64\mingw64\bin\cpp.exe -v
# c:\msys64\mingw64\bin\g++.exe -v
