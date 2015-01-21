mkdir build
cd build

rem Do the compilation
set CFLAGS=-O -g
gcc -c %CFLAGS% ../../../src/grt/grt-cbinding.c
gcc -c %CFLAGS% ../../../src/grt/grt-cvpi.c
gcc -c %CFLAGS% ../../../src/grt/config/clock.c
gcc -c %CFLAGS% ../../../src/ortho/mcode/memsegs_c.c
gcc -c %CFLAGS% -DWITH_GNAT_RUN_TIME ../../../src/grt/config/win32.c
gnatmake %CFLAGS% -gnatn -aI../windows -aI../../../src -aI../../../src/ghdldrv -aI../../../src/psl -aI../../../src/grt -aI../../../src/ortho/mcode -aI../../../src/vhdl -aI../../../src/vhdl/translate ghdl_jit -aI../../../src/ortho -o ghdl.exe -largs grt-cbinding.o clock.o grt-cvpi.o memsegs_c.o win32.o -largs -Wl,--stack,8404992

if errorlevel 1 goto failed

strip ghdl.exe

cd ..
exit /b 0

:failed
echo "Compilation failed"
cd ..
exit /b 1
