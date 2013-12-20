
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
-- $Id: tc1980.vhd,v 1.2 2001-10-26 16:30:14 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p01n02i01980ent IS
END c07s02b02x00p01n02i01980ent;

ARCHITECTURE c07s02b02x00p01n02i01980arch OF c07s02b02x00p01n02i01980ent IS

BEGIN
  TESTING: PROCESS
    -- Define a subtype of a subtype.
    subtype ZERO is NATURAL range 0 to 0;

    -- Define a subtype of a different type.
    type A is range 0 to 10;
    subtype ASUB is A;
    
    -- Define variables of these subtypes.
    variable ZEROV     : ZERO := 0;
    variable AV        : ASUB := 0;
  BEGIN
    -- Failure_here : Should not be legal to compare these two types.
    if (ZEROV = AV) then
      NULL;
    end if;
    assert FALSE 
      report "***FAILED TEST: c07s02b02x00p01n02i01980 - Operands of a relational operator should be of the same type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p01n02i01980arch;
