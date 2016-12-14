Write-Host "========================================" -Foreground Yellow
Write-Host "   Running AppVeyor.GNAT.install.ps1    " -Foreground Yellow
Write-Host "========================================" -Foreground Yellow
Write-Host "Testing for Data Execution Prevention (DEP) Policy setting..."
$cim = Get-CimInstance Win32_OperatingSystem

if ($cim.DataExecutionPrevention_SupportPolicy -eq 3)
{	Write-Host "Data Execution Prevention (DEP) is enabled. Rebooting ..."
	Write-Host "  Disabling DEP via bcdedit"
	bcdedit /set {current} nx AlwaysOff
	Write-Host "  Rebooting ..." -Foreground Yellow
	Restart-Computer -Force
	Start-Sleep -s 5
}
else
{	Write-Host "Data Execution Prevention is disabled."
	Write-Host "  Installing GNAT GPL 2016..." -Foreground Yellow
	Write-Host "  List Git-LFS files"
	Write-Host "  ----------------------------------------"
	git lfs ls-files
	Write-Host "  ----------------------------------------"
	$SilentInstallJob = Start-Job {
		Set-Location $args[0]
		Invoke-Expression ".\dist\git-lfs\gnat-gpl-2016-x86-windows-bin.exe /S"
	} -ArgumentList (Get-Location)
	Wait-Job $SilentInstallJob
	Receive-Job $SilentInstallJob
	Write-Host "----------------------------------------"

	# git lfs install
	git lfs pull
	Write-Host "----------------------------------------"

	dir "c:\gnatpro"
}
