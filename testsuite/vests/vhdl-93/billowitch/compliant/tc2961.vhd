
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
-- $Id: tc2961.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s03b00x00p02n01i02961pkg is
  FUNCTION boo ( PARM_VAL: bit:='1')       RETURN bit;
  FUNCTION boo ( PARM_VAL: bit_vector:="1010")RETURN bit_vector;
  FUNCTION boo ( PARM_VAL: boolean:=TRUE)    RETURN boolean;
  FUNCTION boo ( PARM_VAL: character:='Z')    RETURN character;
  FUNCTION boo ( PARM_VAL: integer:=55)    RETURN integer;
  FUNCTION boo ( PARM_VAL: real:=10.01)    RETURN real;
  FUNCTION boo ( PARM_VAL: string:="STRING")    RETURN string;
  FUNCTION boo ( PARM_VAL: time:=10 ns)    RETURN time;
end c02s03b00x00p02n01i02961pkg;

package body c02s03b00x00p02n01i02961pkg is
  FUNCTION boo ( PARM_VAL: bit:='1') RETURN bit IS
  BEGIN
    assert false report "boo with BIT param" severity note;
    RETURN PARM_VAL;
  END;

  FUNCTION boo ( PARM_VAL: bit_vector:="1010") RETURN bit_vector IS
  BEGIN
    assert false report "boo with BIT_VECTOR param" severity note;
    RETURN PARM_VAL;
  END;

  FUNCTION boo ( PARM_VAL: boolean:=TRUE) RETURN boolean IS
  BEGIN
    assert false report "boo with BOOLEAN param" severity note;
    RETURN PARM_VAL;
  END;

  FUNCTION boo ( PARM_VAL: character:='Z') RETURN character IS
  BEGIN
    assert false report "boo with CHARACTER param" severity note;
    RETURN PARM_VAL;
  END;

  FUNCTION boo ( PARM_VAL: integer:=55) RETURN integer IS
  BEGIN
    assert false report "boo with INTEGER param" severity note;
    RETURN PARM_VAL;
  END;
  
  FUNCTION boo ( PARM_VAL: real:=10.01) RETURN real IS
  BEGIN
    assert false report "boo with REAL param" severity note;
    RETURN PARM_VAL;
  END;
  
  FUNCTION boo ( PARM_VAL: string:="STRING") RETURN string IS
  BEGIN
    assert false report "boo with STRING param" severity note;
    RETURN PARM_VAL;
  END;
  
  FUNCTION boo ( PARM_VAL: time:=10 ns) RETURN time IS
  BEGIN
    assert false report "boo with TIME param" severity note;
    RETURN PARM_VAL;
  END;
end c02s03b00x00p02n01i02961pkg;

ENTITY c02s03b00x00p02n01i02961ent IS
  PORT (bb: INOUT bit;
        bv: INOUT bit_vector(0 TO 3);
        bo: INOUT boolean;
        cc: INOUT character;
        ii: INOUT integer;
        rr: INOUT real;
        ss: INOUT string(1 TO 6);
        tt: INOUT time);
END c02s03b00x00p02n01i02961ent;

use work.c02s03b00x00p02n01i02961pkg.all;
ARCHITECTURE c02s03b00x00p02n01i02961arch OF c02s03b00x00p02n01i02961ent IS
  SIGNAL c1,c2,c3,c4,c5,c6,c7,c8 : INTEGER;
BEGIN
  TESTING: PROCESS
  BEGIN
    WAIT FOR 1 ns;
    bb <= boo;
    bv <= boo;
    bo <= boo;
    cc <= boo;
    ii <= boo;
    rr <= boo;
    ss <= boo;
    tt <= boo;
    
    WAIT FOR 1 ns;
    assert NOT(   (bb = '1')    AND
                  (bv = "1010")    AND
                  (bo = TRUE)    AND
                  (cc = 'Z')    AND
                  (ii = 55)    AND
                  (rr = 10.01)    AND
                  (ss = "STRING") AND
                  (tt = 10 ns))   
      report "***PASSED TEST: c02s03b00x00p02n01i02961"
      severity NOTE;
    assert (   (bb = '1')    AND
               (bv = "1010")    AND
               (bo = TRUE)    AND
               (cc = 'Z')    AND
               (ii = 55)    AND
               (rr = 10.01)    AND
               (ss = "STRING") AND
               (tt = 10 ns))   
      report "***FAILED TEST: c02s03b00x00p02n01i02961 - Overloaded functions test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s03b00x00p02n01i02961arch;
