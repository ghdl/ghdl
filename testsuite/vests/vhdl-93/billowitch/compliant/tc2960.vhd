
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
-- $Id: tc2960.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s03b00x00p02n01i02960pkg is
  FUNCTION boo ( PARM_VAL : bit) RETURN integer;
  FUNCTION boo ( PARM_VAL : bit_vector) RETURN integer;
  FUNCTION boo ( PARM_VAL : boolean) RETURN integer;
  FUNCTION boo ( PARM_VAL : character) RETURN integer;
  FUNCTION boo ( PARM_VAL : integer) RETURN integer;
  FUNCTION boo ( PARM_VAL : real) RETURN integer;
  FUNCTION boo ( PARM_VAL : string) RETURN integer;
  FUNCTION boo ( PARM_VAL : time) RETURN integer;
end c02s03b00x00p02n01i02960pkg;

package body c02s03b00x00p02n01i02960pkg is
  FUNCTION boo ( PARM_VAL : bit) RETURN integer IS
  BEGIN
    assert false report "boo with BIT param" severity note;
    RETURN 1;
  END;

  FUNCTION boo ( PARM_VAL : bit_vector) RETURN integer IS
  BEGIN
    assert false report "boo with BIT_VECTOR param" severity note;
    RETURN 2;
  END;

  FUNCTION boo ( PARM_VAL : boolean) RETURN integer IS
  BEGIN
    assert false report "boo with BOOLEAN param" severity note;
    RETURN 3;
  END;

  FUNCTION boo ( PARM_VAL : character) RETURN integer IS
  BEGIN
    assert false report "boo with CHARACTER param" severity note;
    RETURN 4;
  END;

  FUNCTION boo ( PARM_VAL : integer) RETURN integer IS
  BEGIN
    assert false report "boo with INTEGER param" severity note;
    RETURN 5;
  END;

  FUNCTION boo ( PARM_VAL : real) RETURN integer IS
  BEGIN
    assert false report "boo with REAL param" severity note;
    RETURN 6;
  END;
  
  FUNCTION boo ( PARM_VAL : string) RETURN integer IS
  BEGIN
    assert false report "boo with STRING param" severity note;
    RETURN 7;
  END;
  
  FUNCTION boo ( PARM_VAL : time) RETURN integer IS
  BEGIN
    assert false report "boo with TIME param" severity note;
    RETURN 8;
  END;
end c02s03b00x00p02n01i02960pkg;

ENTITY c02s03b00x00p02n01i02960ent IS
  PORT (bb: INOUT bit;
        bv: INOUT bit_vector(0 TO 3);
        bo: INOUT boolean;
        cc: INOUT character;
        ii: INOUT integer;
        rr: INOUT real;
        ss: INOUT string(1 TO 6);
        tt: INOUT time);
  SUBTYPE bv_4 IS bit_vector(1 TO 4);
  SUBTYPE bv_6 IS bit_vector(1 TO 6);
  
  FUNCTION foo (  PARM_VAL : bv_4) RETURN bit_vector IS
  BEGIN
    assert false report "function foo in entity e" severity note;
    RETURN PARM_VAL;
  END;
END c02s03b00x00p02n01i02960ent;

use work.c02s03b00x00p02n01i02960pkg.all;
ARCHITECTURE c02s03b00x00p02n01i02960arch OF c02s03b00x00p02n01i02960ent IS
  SIGNAL c1,c2,c3,c4,c5,c6,c7,c8 : INTEGER;
BEGIN
  TESTING: PROCESS
  BEGIN
    WAIT FOR 1 ns;
    c1 <= boo(bb);
    c2 <= boo(bv);
    c3 <= boo(bo);
    c4 <= boo(cc);
    c5 <= boo(ii);
    c6 <= boo(rr);
    c7 <= boo(ss);
    c8 <= boo(tt);
    
    WAIT FOR 1 ns;
    assert NOT(   (c1 = 1) AND
                  (c2 = 2) AND
                  (c3 = 3) AND
                  (c4 = 4) AND
                  (c5 = 5) AND
                  (c6 = 6) AND
                  (c7 = 7) AND
                  (c8 = 8)) 
      report "***PASSED TEST: c02s03b00x00p02n01i02960"
      severity NOTE;
    assert (   (c1 = 1) AND
               (c2 = 2) AND
               (c3 = 3) AND
               (c4 = 4) AND
               (c5 = 5) AND
               (c6 = 6) AND
               (c7 = 7) AND
               (c8 = 8)) 
      report "***FAILED TEST: c02s03b00x00p02n01i02960 - Overloaded functions test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s03b00x00p02n01i02960arch;
