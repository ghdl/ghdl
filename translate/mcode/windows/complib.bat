set GHDL=ghdl

cd build
gnatmake -aI..\windows ghdlfilter
cd ..

set REL=..\..\..
set LIBSRC=%REL%\..\..\libraries
set STD_SRCS=textio textio_body
set IEEE_SRCS=std_logic_1164 std_logic_1164_body numeric_std numeric_std-body numeric_bit numeric_bit-body
set VITAL95_SRCS=vital_timing vital_timing_body vital_primitives vital_primitives_body
set VITAL2000_SRCS=timing_p timing_b prmtvs_p prmtvs_b memory_p memory_b

set SYNOPSYS_SRCS=std_logic_arith std_logic_textio std_logic_unsigned std_logic_signed std_logic_misc std_logic_misc-body

mkdir lib
cd lib

mkdir v87
cd v87

mkdir std
cd std
for %%F in (%STD_SRCS%) do %REL%\build\ghdlfilter -v87 < %LIBSRC%\std\%%F.vhdl > %%F.v87 && %REL%\build\%GHDL% -a --std=87 --bootstrap --work=std %%F.v87
cd ..

mkdir ieee
cd ieee
rem Base ieee
for %%F in (%IEEE_SRCS%) do %REL%\build\ghdlfilter -v87 < %LIBSRC%\ieee\%%F.vhdl > %%F.v87 && %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee %%F.v87
rem Vital 95
for %%F in (%VITAL95_SRCS%) do copy %LIBSRC%\vital95\%%F.vhdl %%F.vhd && %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee %%F.vhd
cd ..

mkdir synopsys
cd synopsys
for %%F in (%IEEE_SRCS%) do %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee ..\ieee\%%F.v87
for %%F in (%VITAL95_SRCS%) do %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee ..\ieee\%%F.vhd
for %%F in (%SYNOPSYS_SRCS%) do copy %LIBSRC%\synopsys\%%F.vhdl %%F.vhd && %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee %%F.vhd
cd ..

cd ..
mkdir v93
cd v93

mkdir std
cd std
for %%F in (%STD_SRCS%) do %REL%\build\ghdlfilter -v93 < %LIBSRC%\std\%%F.vhdl > %%F.v93 && %REL%\build\%GHDL% -a --std=93 --bootstrap --work=std %%F.v93
cd ..

mkdir ieee
cd ieee
echo Base ieee
for %%F in (%IEEE_SRCS%) do %REL%\build\ghdlfilter -v93 < %LIBSRC%\ieee\%%F.vhdl > %%F.v93 && %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee %%F.v93
echo Vital 2000
for %%F in (%VITAL2000_SRCS%) do copy %LIBSRC%\vital2000\%%F.vhdl %%F.vhd && %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee %%F.vhd
cd ..

mkdir synopsys
cd synopsys
for %%F in (%IEEE_SRCS%) do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\ieee\%%F.v93
for %%F in (%VITAL2000_SRCS%) do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\ieee\%%F.vhd
for %%F in (%SYNOPSYS_SRCS%) do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\..\v87\synopsys\%%F.vhd
cd ..

cd ..

cd ..