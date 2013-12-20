
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
-- $Id: tc2400.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p08n02i02400ent IS
END c07s03b02x00p08n02i02400ent;

ARCHITECTURE c07s03b02x00p08n02i02400arch OF c07s03b02x00p08n02i02400ent IS

BEGIN
  TESTING: PROCESS
    type t18 is array (1 to 5) of integer;
    variable v18 : t18;
  BEGIN
    v18 := (1 to 2 => 18, others => 0);  -- discrete range in an
                                         -- array aggregate allowed.
    assert NOT(v18(1)=18 and v18(2)=18 and v18(3)=0 and v18(4)=0 and v18(5)=0)
      report "***PASSED TEST: c07s03b02x00p08n02i02400"
      severity NOTE;
    assert (v18(1)=18 and v18(2)=18 and v18(3)=0 and v18(4)=0 and v18(5)=0)
      report "***FAILED TEST: c07s03b02x00p08n02i02400 - An element association with a choice that is a discrete range is allowed in an array aggregate."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p08n02i02400arch;
