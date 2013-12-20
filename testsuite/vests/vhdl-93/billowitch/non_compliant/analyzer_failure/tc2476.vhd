
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
-- $Id: tc2476.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p13n04i02476ent IS
END c07s03b02x02p13n04i02476ent;

ARCHITECTURE c07s03b02x02p13n04i02476arch OF c07s03b02x02p13n04i02476ent IS
  type    index_values is (one, two, three);
  type    ucarr is array (index_values range <>) of Boolean;      
  subtype carr  is ucarr (index_values'low to index_values'high);
  function f2 (i : integer) return carr is
  begin
    return (True, True, TRUE, False);  -- Failure_here      
    -- SEMANTIC ERROR : Last element association specifies
    --  index which is out of bounds for the array.
  end f2;
BEGIN
  TESTING: PROCESS
  BEGIN
    f2(1);
    assert FALSE 
      report "***FAILED TEST: c07s03b02x02p13n04i02476 - Indices are out of bounds for the array."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p13n04i02476arch;
