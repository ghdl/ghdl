
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
-- $Id: tc2959.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c02s03b00x00p02n01i02959pkg is
  FUNCTION boo ( PARM_VAL : bit_vector) RETURN bit;
  FUNCTION boo ( PARM_VAL : bit_vector) RETURN bit_vector;
  FUNCTION boo ( PARM_VAL : bit_vector) RETURN boolean;
  FUNCTION boo ( PARM_VAL : bit_vector) RETURN character;
  FUNCTION boo ( PARM_VAL : bit_vector) RETURN integer;
  FUNCTION boo ( PARM_VAL : bit_vector) RETURN real;
  FUNCTION boo ( PARM_VAL : bit_vector) RETURN string;
  FUNCTION boo ( PARM_VAL : bit_vector) RETURN time;
end c02s03b00x00p02n01i02959pkg;

package body c02s03b00x00p02n01i02959pkg is
  FUNCTION boo (  PARM_VAL : bit_vector) RETURN time IS
  BEGIN
    assert false report "boo with TIME returned" severity note;
    RETURN 10 ns;
  END;

  FUNCTION boo (  PARM_VAL : bit_vector) RETURN string IS
  BEGIN
    assert false report "boo with STRING returned" severity note;
    RETURN "STRING";
  END;

  FUNCTION boo (  PARM_VAL : bit_vector) RETURN real IS
  BEGIN
    assert false report "boo with REAL returned" severity note;
    RETURN 10.01;
  END;

  FUNCTION boo (  PARM_VAL : bit_vector) RETURN integer IS
  BEGIN
    assert false report "boo with INTEGER returned" severity note;
    RETURN 55;
  END;

  FUNCTION boo (  PARM_VAL : bit_vector) RETURN character IS
  BEGIN
    assert false report "boo with CHARACTER returned" severity note;
    RETURN 'Z';
  END;

  FUNCTION boo (  PARM_VAL : bit_vector) RETURN boolean IS
  BEGIN
    assert false report "boo with BOOLEAN returned" severity note;
    RETURN TRUE;
  END;
  
  FUNCTION boo (  PARM_VAL : bit_vector) RETURN bit_vector IS
  BEGIN
    assert false report "boo with BIT_VECTOR returned" severity
      note;
    RETURN "1010";
  END;
  
  FUNCTION boo (  PARM_VAL : bit_vector) RETURN bit IS
  BEGIN
    assert false report "boo with BIT returned" severity note;
    RETURN '1';
  END;
end c02s03b00x00p02n01i02959pkg;

ENTITY c02s03b00x00p02n01i02959ent IS
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
END c02s03b00x00p02n01i02959ent;

use work.c02s03b00x00p02n01i02959pkg.all;
ARCHITECTURE c02s03b00x00p02n01i02959arch OF c02s03b00x00p02n01i02959ent IS
  SIGNAL c1 : bv_4;
BEGIN
  TESTING: PROCESS
  BEGIN
    WAIT FOR 1 ns;
    c1 <= boo ( bv_6'(OTHERS => '1'));
    bb <= boo (c1);
    bv <= boo (c1);
    bo <= boo (c1);
    cc <= boo (c1);
    ii <= boo (c1);
    rr <= boo (c1);
    ss <= boo (c1);
    tt <= boo (c1);
    
    WAIT FOR 1 ns;
    assert NOT(    (c1 = "1010") AND
                   (bb = '1') AND
                   (bv = "1010") AND
                   (bo = TRUE) AND
                   (cc = 'Z') AND
                   (ii = 55) AND
                   (rr = 10.01) AND
                   (ss = "STRING") AND
                   (tt = 10 ns))
      report "***PASSED TEST: c02s03b00x00p02n01i02959"
      severity NOTE;
    assert (    (c1 = "1010") AND
                (bb = '1') AND
                (bv = "1010") AND
                (bo = TRUE) AND
                (cc = 'Z') AND
                (ii = 55) AND
                (rr = 10.01) AND
                (ss = "STRING") AND
                (tt = 10 ns))
      report "***FAILED TEST: c02s03b00x00p02n01i02959 - Overloaded functions test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s03b00x00p02n01i02959arch;
