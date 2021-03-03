mkdir build
cd build

rem Do the compilation
set CFLAGS= -O -Wall

gcc -c %CFLAGS% ../../../src/grt/grt-cstdio.c
if errorlevel 1 goto failed

gcc -c %CFLAGS% ../../../src/grt/grt-cvpi.c
if errorlevel 1 goto failed

gcc -c %CFLAGS% ../../../src/grt/grt-cvhpi.c
if errorlevel 1 goto failed

gcc -c %CFLAGS% ../../../src/grt/config/clock.c
if errorlevel 1 goto failed

gcc -c %CFLAGS% ../../../src/ortho/mcode/memsegs_c.c
if errorlevel 1 goto failed

gcc -c %CFLAGS% -DWITH_GNAT_RUN_TIME ../../../src/grt/config/win32.c
if errorlevel 1 goto failed

gnatmake %CFLAGS% -gnatn -aI../windows -aI../../../src -aI../../../src/ghdldrv -aI../../../src/psl -aI../../../src/grt -aI../../../src/ortho/mcode -aI../../../src/vhdl -aI../../../src/vhdl/translate ghdl_jit -aI../../../src/ortho -o ghdl.exe -largs grt-cstdio.o clock.o grt-cvpi.o memsegs_c.o win32.o -ldbghelp -Wl,--stack,8404992
if errorlevel 1 goto failed

cd ..
exit /b 0

:failed
echo "Compilation failed"
cd ..
exit /b 1
