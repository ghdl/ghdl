GHDL 0.31-mcode index out of range behavior 

---------------------
Information

GHDL     : GHDL-0.31 release source tree
COMPILER : GNAT 2013
OS       : XP SP3 32 bit

The attached zip file contains two testcases for index out of range.

---------------------
index_range_test_A.vhd 

This test indexes off the end of an unconstrained port of type bit_vector.

This version exits abnormally, with no message, for GHDL-0.31-mcode on Windows:

    C:\brian\jobs\ghdl_test\test_exceptions>ghdl -r index_range_test_A

    This application has requested the Runtime to terminate it in an unusual way.
    Please contact the application's support team for more information.

In contrast, ghdl-gcc 0.27 prints the offending code line number:

    $ ghdl -r index_range_test_A
    ./index_range_test_a:error: bound check failure at index_range_test_A.vhd:19
    ghdl: compilation error


The same abnormal exit behavior is seen for ghdl-mcode when running test suite gna/bug16782

---------------------
index_range_test_B.vhd 

This test indexes off the end of a bit_vector signal of static width.

This version exits with an overflow message for GHDL-0.31-mcode:

    C:\brian\jobs\ghdl_test\test_exceptions>ghdl -r index_range_test_B
    c:\ghdl\ghdl-0.31\bin\ghdl.exe:error: overflow detected
    c:\ghdl\ghdl-0.31\bin\ghdl.exe:error: simulation failed


In contrast, ghdl-gcc 0.27 prints the offending code line number:

    $ ghdl -r index_range_test_B
    ./index_range_test_b:error: bound check failure at index_range_test_B.vhd:22
    ./index_range_test_b:error: simulation failed
    ghdl: compilation error


---------------------
Related ghdl-discuss messages:

https://mail.gna.org/public/ghdl-discuss/2014-01/msg00233.html
https://mail.gna.org/public/ghdl-discuss/2014-01/msg00237.html

---------------------
zip file contents:

  readme.txt : this file

  index_range_test_A.vhd
  index_range_test_A.ghdl-0.31-mcode.win32.log

  index_range_test_B.vhd
  index_range_test_B.ghdl-0.31-mcode.win32.log
