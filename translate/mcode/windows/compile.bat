mkdir build
cd build

rem Do the compilation
set CFLAGS=-O -g
gcc -c %CFLAGS% ../../grt/grt-cbinding.c
gcc -c %CFLAGS% ../../grt/grt-cvpi.c
gcc -c %CFLAGS% ../../grt/config/clock.c
gcc -c %CFLAGS% ../../../ortho/mcode/memsegs_c.c
gcc -c %CFLAGS% -DWITH_GNAT_RUN_TIME ../../grt/config/win32.c
gnatmake %CFLAGS% -gnatn -aI../windows -aI../../.. -aI../.. -aI../../ghdldrv -aI../../grt -aI../../../ortho/mcode ghdl_mcode -o ghdl.exe -largs grt-cbinding.o clock.o grt-cvpi.o memsegs_c.o win32.o -largs -Wl,--stack,8404992

if errorlevel 1 goto failed

strip ghdl.exe

cd ..
exit /b 0

:failed
echo "Compilation failed"
cd ..
exit /b 1
