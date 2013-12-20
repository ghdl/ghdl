
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
-- $Id: tc1167.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s06b00x00p04n02i01167ent IS
END c06s06b00x00p04n02i01167ent;

ARCHITECTURE c06s06b00x00p04n02i01167arch OF c06s06b00x00p04n02i01167ent IS
  attribute p: POSITIVE;
  signal    s: integer;
  attribute p of s: signal is 10;
BEGIN
  TESTING: PROCESS
  BEGIN
    if s'p = 10 then  -- Success_here
      s <=  12;
    else
      s <= 0;
    end if;
    wait for 10 ns;
    assert NOT( s=12 )
      report "***PASSED TEST: c06s06b00x00p04n02i01167"
      severity NOTE;
    assert ( s=12 )
      report "***FAILED TEST: c06s06b00x00p04n02i01167 - The meaning of the prefix of an attribute must be determinable independently of the attribute designator and independently of the fact that it is the prefix of an attribute."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s06b00x00p04n02i01167arch;
