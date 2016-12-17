Write-Host "Installing dependencies ..." -Foreground Yellow
C:\msys64\usr\bin\pacman -V
# list installed packages and versions
# C:\msys64\usr\bin\pacman -Q
C:\msys64\usr\bin\pacman -S mingw-w64-x86_64-llvm35 mingw-w64-x86_64-clang35 --noconfirm

exit $LastExitCode
