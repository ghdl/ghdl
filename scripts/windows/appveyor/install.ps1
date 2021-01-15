Write-Host "Installing dependencies ..." -ForegroundColor Yellow
Write-Host "----------------------------------------" -ForegroundColor Yellow
Write-Host "Installing MinGW64 packages ..." -ForegroundColor Yellow

C:\msys64\usr\bin\pacman -V
# list installed packages and versions
# C:\msys64\usr\bin\pacman -Q

if ($env:BUILD_MINGW -eq "mingw32")
{	Write-Host "Installing MinGW32 packages ..." -ForegroundColor Yellow
	if ($env:BUILD_BACKEND -eq "mcode")
	{
	}
	elseif ($env:BUILD_BACKEND -eq "llvm")
	{	C:\msys64\usr\bin\pacman -S mingw-w64-i686-llvm mingw-w64-i686-clang --noconfirm
	}
}
elseif ($env:BUILD_MINGW -eq "mingw64")
{	Write-Host "Installing MinGW64 packages ..." -ForegroundColor Yellow
	if ($env:BUILD_BACKEND -eq "mcode")
	{
	}
	elseif ($env:BUILD_BACKEND -eq "llvm")
	{	C:\msys64\usr\bin\pacman -S mingw-w64-x86_64-llvm mingw-w64-x86_64-clang --noconfirm
	}
}

Write-Host "Installing NuGet as PackageProvider ..." -ForegroundColor Yellow
Install-PackageProvider NuGet -Force
Import-PackageProvider NuGet -Force
Set-PSRepository -Name PSGallery -InstallationPolicy Trusted

Write-Host "Installing PowerShell modules ..." -ForegroundColor Yellow
Install-Module Pscx -AllowClobber

#Write-Host "Check all Write-* CmdLets ..." -ForegroundColor Yellow
#Get-Command -Verb Write | Format-Table

exit $LastExitCode
