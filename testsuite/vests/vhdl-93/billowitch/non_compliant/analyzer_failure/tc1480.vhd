
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
-- $Id: tc1480.vhd,v 1.2 2001-10-26 16:30:10 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s08b00x00p04n01i01480ent IS
END c08s08b00x00p04n01i01480ent;

ARCHITECTURE c08s08b00x00p04n01i01480arch OF c08s08b00x00p04n01i01480ent IS

BEGIN
  TESTING: PROCESS

    function f return real is
      type    t1 is (one,two,three,four);
      subtype    st is t1 range one to three;
      variable    v  : st := one;
    begin
      case v is
        when one =>
          return 0.1;
        when two to four =>       -- error : range violates constraints
          return 9.0;
      end case;
    end f;

  BEGIN

    assert FALSE 
      report "***FAILED TEST: c08s08b00x00p04n01i01480 - Static range violation." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s08b00x00p04n01i01480arch;
