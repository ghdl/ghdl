
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
-- $Id: tc141.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p09n01i00141ent IS
  FUNCTION addup (i1,i2,i3:INTEGER:=5) RETURN INTEGER IS
  BEGIN
    RETURN (i1+i2+i3);
  END;
END c04s03b02x02p09n01i00141ent;

ARCHITECTURE c04s03b02x02p09n01i00141arch OF c04s03b02x02p09n01i00141ent IS
  SIGNAL a1 : INTEGER := 57;
  SIGNAL a2 : INTEGER := 68;
  SIGNAL a3 : INTEGER := 77;
  SIGNAL i1 : INTEGER := 0;
  SIGNAL i2 : INTEGER := 0;
  SIGNAL i3 : INTEGER := 0;
  SIGNAL i4 : INTEGER := 0;
  SIGNAL i5 : INTEGER := 0;
BEGIN
  TESTING: PROCESS
  BEGIN
    WAIT FOR 1 ns;
    i1 <= addup(i2=>a1,i1=>a1);
    WAIT FOR 1 ns;
    IF (i1 = 119) THEN
      ASSERT false REPORT "PASS: Function call uses same actual twice plus default" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY error;
    END IF;
    
    WAIT FOR 1 ns;
    i2 <= addup(i3=>a2,i1=>a2,i2=>a1);
    WAIT FOR 1 ns;
    IF (i2 = 193) THEN
      ASSERT false REPORT "PASS: Function call uses same actual twice" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY error;
    END IF;
    
    WAIT FOR 1 ns;
    i3 <= addup(i3=>a3,i2=>a3,i1=>a3);
    WAIT FOR 1 ns;
    IF (i3 = 231) THEN
      ASSERT false REPORT "PASS: Function call uses same actual thrice" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY error;
    END IF;
    
    WAIT FOR 1 ns;
    i4 <= addup;
    WAIT FOR 1 ns;
    IF (i4 = 15) THEN
      ASSERT false REPORT "PASS: All parameters defaulted to same value" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY error;
    END IF;
    
    WAIT FOR 1 ns;
    i5 <= addup(addup(addup,addup,addup),addup(addup,addup,addup),addup(addup,addup,addup));
    WAIT FOR 1 ns;
    IF (i5 = 135) THEN
      ASSERT false REPORT "PASS: All parameters defaulted to same value recursively" SEVERITY note;
    ELSE
      ASSERT false REPORT "FAIL: Function call fails" SEVERITY error;
    END IF;
    wait for 5 ns;
    assert NOT(   i1 = 119   and
                  i2 = 193   and
                  i3 = 231   and
                  i4 = 15      and
                  i5 = 135   ) 
      report "***PASSED TEST: c04s03b02x02p09n01i00141"
      severity NOTE;
    assert (   i1 = 119   and
               i2 = 193   and
               i3 = 231   and
               i4 = 15      and
               i5 = 135   ) 
      report "***FAILED TEST: c04s03b02x02p09n01i00141 - Named association on function call test failed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p09n01i00141arch;
