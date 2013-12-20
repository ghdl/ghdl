
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
-- $Id: tc2480.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p13n04i02480ent IS
END c07s03b02x02p13n04i02480ent;

ARCHITECTURE c07s03b02x02p13n04i02480arch OF c07s03b02x02p13n04i02480ent IS
  type    UNCONSTRAINED_ARRAY is array ( integer range <> ) of character;
  subtype CA_UP   is UNCONSTRAINED_ARRAY (1 to 10);
  subtype CA_DOWN is UNCONSTRAINED_ARRAY (10 downto 1);
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT((CA_DOWN'RIGHT = 1) and (CA_UP'RIGHT = 10))
      report "***PASSED TEST: c07s03b02x02p13n04i02480" 
      severity NOTE;
    assert ((CA_DOWN'RIGHT = 1) and (CA_UP'RIGHT = 10))
      report "***FAILED TEST: c07s03b02x02p13n04i02480 - Rightmost bounds INCORRECT for positional aggregates." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p13n04i02480arch;
