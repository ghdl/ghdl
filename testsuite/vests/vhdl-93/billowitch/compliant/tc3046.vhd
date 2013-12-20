
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
-- $Id: tc3046.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s02b03x00p01n02i03046ent IS
  type c_r is
    record
      i1 : integer;
      r1 : real;
      b1 : bit;
    end record;
  type c_a is array(1 to 3) of bit;
END c12s02b03x00p01n02i03046ent;

ARCHITECTURE c12s02b03x00p01n02i03046arch OF c12s02b03x00p01n02i03046ent IS
  signal sr : c_r := (14,1.4,'1');
  signal sa : c_a := "101";
BEGIN
  b1: block
    port(r:c_r; a:c_a);
    port map (r=>sr, a=>sa);
  begin
    TESTING: PROCESS
    BEGIN
      wait for 5 ns;
      assert NOT( r=(14,1.4,'1') and a="101")
        report "***PASSED TEST: c12s02b03x00p01n02i03046"
        severity NOTE;
      assert ( r=(14,1.4,'1') and a="101")
        report "***FAILED TEST: c12s02b03x00p01n02i03046 - Ports should conform to their subtype indication."
        severity ERROR;
      wait;
    END PROCESS TESTING;
  end block;

END c12s02b03x00p01n02i03046arch;
