Write-Host "Installing dependencies ..." -Foreground Yellow
Write-Host "----------------------------------------" -Foreground Yellow

C:\msys64\usr\bin\pacman.exe -V
# list installed packages and versions
# C:\msys64\usr\bin\pacman.exe -Q

cd $env:APPVEYOR_BUILD_FOLDER

if ($env:BUILD_BACKEND -eq "gcc")
{	Write-Host "Installing common MinGW packages ..." -Foreground Yellow
	C:\msys64\usr\bin\pacman.exe -S tar --noconfirm
}

if ($env:BUILD_MINGW -eq "mingw32")
{	Write-Host "Installing MinGW32 packages ..." -Foreground Yellow
	if ($env:BUILD_BACKEND -eq "mcode")
	{
	}
	elseif ($env:BUILD_BACKEND -eq "llvm")
	{	C:\msys64\usr\bin\pacman.exe -S mingw-w64-i686-llvm35 mingw-w64-i686-clang35 --noconfirm
	}
}
elseif ($env:BUILD_MINGW -eq "mingw64")
{	Write-Host "Installing MinGW64 packages ..." -Foreground Yellow
	if ($env:BUILD_BACKEND -eq "mcode")
	{
	}
	elseif ($env:BUILD_BACKEND -eq "llvm")
	{	C:\msys64\usr\bin\pacman.exe -S mingw-w64-x86_64-llvm35 mingw-w64-x86_64-clang35 --noconfirm
	}
}

if ($env:BUILD_BACKEND -eq "gcc")
{	# Assembles paths
	$GCC_SOURCE_DIR =     "$($env:APPVEYOR_BUILD_FOLDER)\gcc-$BUILD_GCC_VERSION"
	# Export GCC paths as environment variable
	$env:GCC_SOURCE_DIR = $GCC_SOURCE_DIR

	cd $env:APPVEYOR_BUILD_FOLDER
	Write-Host "Downloading GCC $BUILD_GCC_VERSION sources ..." -Foreground Yellow
	wget https://ftp.gnu.org/gnu/gcc/gcc-$BUILD_GCC_VERSION/gcc-$BUILD_GCC_VERSION.tar.gz
	Write-Host "Extracting GCC sources ..." -Foreground Yellow
	tar xzf gcc-$BUILD_GCC_VERSION.tar.gz
}

Write-Host "Installing NuGet as PackageProvider ..." -Foreground Yellow
Install-PackageProvider NuGet -Force
Import-PackageProvider NuGet -Force
Set-PSRepository -Name PSGallery -InstallationPolicy Trusted

Write-Host "Installing PowerShell modules ..." -Foreground Yellow
Install-Module Pscx -AllowClobber


cd $env:APPVEYOR_BUILD_FOLDER
exit $LastExitCode
