
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
-- $Id: tc2085.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02085ent IS
END c07s02b04x00p20n01i02085ent;

ARCHITECTURE c07s02b04x00p20n01i02085arch OF c07s02b04x00p20n01i02085ent IS

  TYPE    boolean_v    is array (integer range <>) of boolean;
  SUBTYPE     boolean_4_up     is boolean_v (1 to 4);
  SUBTYPE     boolean_4_dwn     is boolean_v (4 downto 1);
  SUBTYPE     boolean_8_dwn     is boolean_v (4 downto -3);

BEGIN
  TESTING: PROCESS
    variable r_operand : boolean_4_up := (true, true, false, false);
    variable l_operand : boolean_4_dwn:= (false, false, true, true);
    variable result    : boolean_8_dwn;
  BEGIN

    result := l_operand & r_operand;
    assert ( result (4)  = false )
      report  "result (4) /= false" severity FAILURE;
    assert ( result (1)  = true )
      report  "result (1) /= true" severity FAILURE;
    assert ( result (0)  = true )
      report  "result (0) /= true" severity FAILURE;
    assert ( result (-3)  = false )
      report  "result (-3) /= false" severity FAILURE;

    assert NOT((result(4)=false) and (result=(false,false,true,true,true,true,false,false)))
      report "***PASSED TEST: c07s02b04x00p20n01i02085"
      severity NOTE;
    assert ((result(4)=false) and (result=(false,false,true,true,true,true,false,false)))
      report "***FAILED TEST: c07s02b04x00p20n01i02085 - Concatenated array is descending and that the left bound is that of the first operand."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02085arch;
