mkdir build
cd build

rem Do the compilation
gcc -c -O ../grt/grt-cbinding.c
gcc -c -O ../grt/grt-cvpi.c
gcc -c -O ../grt/config/clock.c
gcc -c -O ../ortho/memsegs_c.c
gcc -c -O -DWITH_GNAT_RUN_TIME ../grt/config/win32.c
gnatmake -O -gnatn -aI../windows -aI../ghdl -aI../ghdldrv -aI../grt -aI../ortho ghdl_mcode -o ghdl.exe -largs grt-cbinding.o clock.o grt-cvpi.o memsegs_c.o win32.o
strip ghdl.exe

cd ..
