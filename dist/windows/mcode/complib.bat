set GHDL=ghdl

cd build
gnatmake -aI..\windows ghdlfilter
cd ..

set REL=..\..\..
set LIBSRC=%REL%\..\..\libraries

::
:: library sources
::
set STD_SRCS=       textio textio-body
set IEEE_SRCS=      std_logic_1164 std_logic_1164-body numeric_std numeric_std-body numeric_bit numeric_bit-body
set MATH_SRCS=      math_real math_real-body math_complex math_complex-body

set STD08_SRCS=     textio textio-body env env-body
set IEEE08_SRCS=    std_logic_1164 std_logic_1164-body std_logic_textio math_real math_real-body math_complex math_complex-body numeric_bit numeric_bit-body numeric_bit_unsigned numeric_bit_unsigned-body numeric_std numeric_std-body numeric_std_unsigned numeric_std_unsigned-body fixed_float_types fixed_generic_pkg fixed_generic_pkg-body fixed_pkg float_generic_pkg float_generic_pkg-body float_pkg ieee_bit_context ieee_std_context

set VITAL95_SRCS=   vital_timing vital_timing-body vital_primitives vital_primitives-body
set VITAL2000_SRCS= timing_p timing_b prmtvs_p prmtvs_b memory_p memory_b

set SYNOPSYS_SRCS=  std_logic_arith std_logic_textio std_logic_unsigned std_logic_signed std_logic_misc std_logic_misc-body
set MENTOR_SRCS=    std_logic_arith std_logic_arith-body


mkdir lib
cd lib

:::::::::::::::::
echo v87 libraries...

mkdir v87
cd v87

echo std
mkdir std
cd std
for %%F in (%STD_SRCS%)      do %REL%\build\ghdlfilter -v87 < %LIBSRC%\std\%%F.vhdl > %%F.v87   && %REL%\build\%GHDL% -a --std=87 --bootstrap --work=std %%F.v87
cd ..

echo ieee
mkdir ieee
cd ieee
for %%F in (%IEEE_SRCS%)     do %REL%\build\ghdlfilter -v87 < %LIBSRC%\ieee\%%F.vhdl > %%F.v87  && %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee %%F.v87
for %%F in (%VITAL95_SRCS%)  do copy %LIBSRC%\vital95\%%F.vhdl %%F.vhd                          && %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee %%F.vhd
cd ..

echo synopsys
mkdir synopsys
cd synopsys
for %%F in (%IEEE_SRCS%)     do %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee ..\ieee\%%F.v87
for %%F in (%VITAL95_SRCS%)  do %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee ..\ieee\%%F.vhd
for %%F in (%SYNOPSYS_SRCS%) do copy %LIBSRC%\synopsys\%%F.vhdl %%F.vhd  && %REL%\build\%GHDL% -a --std=87 -P..\std --work=ieee %%F.vhd
cd ..

cd ..

:::::::::::::::::
echo v93 libraries...

mkdir v93
cd v93

echo std
mkdir std
cd std
for %%F in (%STD_SRCS%)       do %REL%\build\ghdlfilter -v93 < %LIBSRC%\std\%%F.vhdl > %%F.v93  && %REL%\build\%GHDL% -a --std=93 --bootstrap --work=std %%F.v93
cd ..

echo ieee
mkdir ieee
cd ieee
for %%F in (%IEEE_SRCS%)      do %REL%\build\ghdlfilter -v93 < %LIBSRC%\ieee\%%F.vhdl > %%F.v93 && %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee %%F.v93
for %%F in (%VITAL2000_SRCS%) do copy %LIBSRC%\vital2000\%%F.vhdl %%F.vhd                       && %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee %%F.vhd
for %%F in (%MATH_SRCS%)      do copy %LIBSRC%\ieee\%%F.vhdl %%F.vhd                            && %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee %%F.vhd
cd ..

echo synopsys
mkdir synopsys
cd synopsys
for %%F in (%IEEE_SRCS%)      do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\ieee\%%F.v93
for %%F in (%VITAL2000_SRCS%) do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\ieee\%%F.vhd
for %%F in (%MATH_SRCS%)      do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\ieee\%%F.vhd
for %%F in (%SYNOPSYS_SRCS%)  do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\..\v87\synopsys\%%F.vhd
cd ..

echo mentor
mkdir mentor
cd mentor
for %%F in (%IEEE_SRCS%)      do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\ieee\%%F.v93
for %%F in (%VITAL2000_SRCS%) do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\ieee\%%F.vhd
for %%F in (%MATH_SRCS%)      do %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee ..\ieee\%%F.vhd
for %%F in (%MENTOR_SRCS%)    do copy %LIBSRC%\mentor\%%F.vhdl %%F.vhd  && %REL%\build\%GHDL% -a --std=93 -P..\std --work=ieee %%F.vhd
cd ..

cd ..

:::::::::::::::::
echo v08 libraries...

mkdir v08
cd v08

echo std
mkdir std
cd std
for %%F in (%STD08_SRCS%)     do %REL%\build\ghdlfilter -v08 < %LIBSRC%\std\%%F.vhdl > %%F.v08 && %REL%\build\%GHDL% -a --std=08 --bootstrap --work=std %%F.v08
cd ..

echo ieee
mkdir ieee
cd ieee
for %%F in (%IEEE08_SRCS%)    do %REL%\build\ghdlfilter -v08 < %LIBSRC%\ieee2008\%%F.vhdl > %%F.v08 && %REL%\build\%GHDL% -a --std=08 -P..\std --work=ieee %%F.v08
cd ..

:::::::::::::::::

cd ..\..
