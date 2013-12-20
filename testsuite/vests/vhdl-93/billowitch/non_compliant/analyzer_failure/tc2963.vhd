
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: tc2963.vhd,v 1.2 2001-10-26 16:30:24 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s03b00x00p03n01i02963pkg is
  FUNCTION boo (P1:integer;X:bit:='1') RETURN integer;
  FUNCTION boo (P2:integer;X:bit_vector:="1010") RETURN integer;
  FUNCTION boo (P3:integer;X:boolean:=TRUE) RETURN integer;
  FUNCTION boo (P4:integer;X:character:='Z') RETURN integer;
  FUNCTION boo (P5:integer;X:integer:=55) RETURN integer;
  FUNCTION boo (P6:integer;X:real:=10.01) RETURN integer;
  FUNCTION boo (P7:integer;X:string:="STRING") RETURN integer;
  FUNCTION boo (P8:integer;X:time:=10 ns) RETURN integer;
end c02s03b00x00p03n01i02963pkg;

package bodyc02s03b00x00p03n01i02963pkg is
  FUNCTION boo (P1:integer;X:bit:='1') RETURN integer IS
  BEGIN
    assert false report "boo with BIT param" severity note;
    RETURN 1;
  END;

  FUNCTION boo (P2:integer;X:bit_vector:="1010") RETURN integer IS    
  BEGIN
    assert false report "boo with BIT_VECTOR param" severity note;
    RETURN 2;
  END;

  FUNCTION boo (P3:integer;X:boolean:=TRUE) RETURN integer IS
  BEGIN
    assert false report "boo with BOOLEAN param" severity note;
    RETURN 3;
  END;

  FUNCTION boo (P4:integer;X:character:='Z') RETURN integer IS
  BEGIN
    assert false report "boo with CHARACTER param" severity note;
    RETURN 4;
  END;

  FUNCTION boo (P5:integer;X:integer:=55) RETURN integer IS
  BEGIN
    assert false report "boo with INTEGER param" severity note;
    RETURN 5;
  END;

  FUNCTION boo (P6:integer;X:real:=10.01) RETURN integer IS
  BEGIN
    assert false report "boo with REAL param" severity note;
    RETURN 6;
  END;
  
  FUNCTION boo (P7:integer;X:string:="STRING") RETURN integer IS
  BEGIN
    assert false report "boo with STRING param" severity note;
    RETURN 7;
  END;
  
  FUNCTION boo (P8:integer;X:time:=10 ns) RETURN integer IS
  BEGIN
    assert false report "boo with TIME param" severity note;
    RETURN 8;
  END;
end c02s03b00x00p03n01i02963pkg;

ENTITY c02s03b00x00p03n01i02963ent IS
  PORT (b1,b2,b3,b4,b5,b6,b7,b8: INOUT integer);
END c02s03b00x00p03n01i02963ent;

use work.c02s03b00x00p03n01i02963pkg.all;
ARCHITECTURE c02s03b00x00p03n01i02963arch OF c02s03b00x00p03n01i02963ent IS
  SIGNAL c1,c2,c3,c4,c5,c6,c7,c8 : INTEGER;
BEGIN
  TESTING: PROCESS
  BEGIN
    WAIT FOR 1 ns;
    c1 <= boo(b1);
    c2 <= boo(b2);
    c3 <= boo(b3);
    c4 <= boo(b4);
    c5 <= boo(b5);
    c6 <= boo(b6);
    c7 <= boo(b7);
    c8 <= boo(b8);
    wait for 5 ns;

    assert FALSE
      report "***FAILED TEST: c02s03b00x00p03n01i02963 - A call to an overloaded subprogram is ambiguous."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s03b00x00p03n01i02963arch;
