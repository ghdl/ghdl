call windows\compile
if errorlevel 1 goto end
call windows\complib
if errorlevel 1 goto end
"f:\Program Files\NSIS\makensis" windows\ghdl.nsi
if errorlevel 1 goto end

gnatmake windows/ghdlversion -o windows/ghdlversion.exe
windows\ghdlversion < ../../version.ads > windows/version.nsi

exit /b 0

:end
echo "Error during compilation"
exit /b 1

