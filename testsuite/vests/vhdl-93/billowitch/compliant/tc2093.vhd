
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
-- $Id: tc2093.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02093ent IS
END c07s02b04x00p20n01i02093ent;

ARCHITECTURE c07s02b04x00p20n01i02093arch OF c07s02b04x00p20n01i02093ent IS

  TYPE    boolean_v    is array (integer range <>) of boolean;
  SUBTYPE     boolean_4_dwn     is boolean_v (4 downto 1);
  SUBTYPE     boolean_4_null  is boolean_v (4 downto 5);
  SUBTYPE     boolean_8_dwn     is boolean_v (8 downto 1);

BEGIN
  TESTING: PROCESS
    variable l_operand : boolean_4_null ;
    variable r_operand : boolean_4_dwn  := (false, false, true, true);
    variable result    : boolean_4_dwn;
  BEGIN

    result := l_operand & r_operand;
    assert ( result (4)  = false )
      report  "result (4) /= false" severity FAILURE;
    assert ( result (1)  = true )
      report  "result (1) /= true" severity FAILURE;

    assert NOT((result(4)=false) and (result=(false,false,true,true)))
      report "***PASSED TEST: c07s02b04x00p20n01i02093"
      severity NOTE;
    assert ((result(4)=false) and (result=(false,false,true,true)))
      report "***FAILED TEST: c07s02b04x00p20n01i02093 - The left bound of the concatenated array is that of the second operand."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02093arch;
