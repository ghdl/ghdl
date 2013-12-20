
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
-- $Id: tc2474.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x02p13n02i02474ent IS
END c07s03b02x02p13n02i02474ent;

ARCHITECTURE c07s03b02x02p13n02i02474arch OF c07s03b02x02p13n02i02474ent IS
  type     UNCONSTRAINED_ARRAY is array ( integer range <> ) of character;
  subtype  CA_DOWN is UNCONSTRAINED_ARRAY (10 downto 1);
BEGIN
  TESTING: PROCESS
    variable k : CA_DOWN;
  BEGIN
    k := CA_DOWN'((10 downto 1 => 'B'));   
    assert NOT(k="BBBBBBBBBB") 
      report "***PASSED TEST: c07s03b02x02p13n02i02474"
      severity NOTE;
    assert (k="BBBBBBBBBB") 
      report "***FAILED TEST: c07s03b02x02p13n02i02474 - The range of the subtype of the aggregate array is not the same as that of the index subtype of the base subtype of the aggregate."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x02p13n02i02474arch;
