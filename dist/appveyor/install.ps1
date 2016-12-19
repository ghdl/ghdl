Write-Host "Installing dependencies ..." -Foreground Yellow
Write-Host "----------------------------------------" -Foreground Yellow
Write-Host "Installing MinGW64 packages ..." -Foreground Yellow
C:\msys64\usr\bin\pacman -V
# list installed packages and versions
# C:\msys64\usr\bin\pacman -Q
C:\msys64\usr\bin\pacman -S mingw-w64-x86_64-llvm35 mingw-w64-x86_64-clang35 --noconfirm

Write-Host "Installing NuGet as PackageProvider ..." -Foreground Yellow
Install-PackageProvider NuGet -Force
Import-PackageProvider NuGet -Force
Set-PSRepository -Name PSGallery -InstallationPolicy Trusted

Write-Host "Installing PowerShell modules ..." -Foreground Yellow
Install-Module Pscx -AllowClobber

#Write-Host "Check all Write-* CmdLets ..." -Foreground Yellow
#Get-Command -Verb Write | Format-Table

dir c:\MinGW\bin
dir c:\MinGW\msys\1.0
dir c:\msys64
#exit $LastExitCode
exit 0
