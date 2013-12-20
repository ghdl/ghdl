
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
-- $Id: tc2086.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02086ent IS
END c07s02b04x00p20n01i02086ent;

ARCHITECTURE c07s02b04x00p20n01i02086arch OF c07s02b04x00p20n01i02086ent IS

  TYPE    boolean_v    is array (integer range <>) of boolean;
  SUBTYPE     boolean_4_up     is boolean_v (1 to 4);
  SUBTYPE     boolean_4_null  is boolean_v (4 to 3);
  SUBTYPE     boolean_8_up     is boolean_v (1 to 8);

BEGIN
  TESTING: PROCESS
    variable l_operand : boolean_4_null ;
    variable r_operand : boolean_4_up   := (false, false, true, true);
    variable result    : boolean_4_up;
  BEGIN

    result := l_operand & r_operand;
    assert ( result (1)  = false )
      report  "result (1) /= false" severity FAILURE;
    assert ( result (4)  = true )
      report  "result (4) /= true" severity FAILURE;

    assert NOT((result(1)=false) and (result=(false,false,true,true)))
      report "***PASSED TEST: c07s02b04x00p20n01i02086"
      severity NOTE;
    assert ((result(1)=false) and (result=(false,false,true,true)))
      report "***FAILED TEST: c07s02b04x00p20n01i02086 - The left bound of the concatenated array is that of the second operand."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02086arch;
