
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
-- $Id: tc3047.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s02b03x00p01n02i03047ent IS
END c12s02b03x00p01n02i03047ent;

ARCHITECTURE c12s02b03x00p01n02i03047arch OF c12s02b03x00p01n02i03047ent IS
  signal si : integer;
  signal sr : real;
  signal sb : bit;
BEGIN
  si <= 4    after 1 ns;
  sr <= 3.2    after 1 ns;
  sb <= '1'    after 1 ns;
  b1: block
    port (i:in integer := 3; r:in real := 4.5; b:in bit := '0');
    port map (i=>si, r=>sr, b=>sb);
  begin
    TESTING: PROCESS
    BEGIN
      wait for 5 ns;
      assert NOT( i=4 and r=3.2 and b='1')
        report "***PASSED TEST: c12s02b03x00p01n02i03047"
        severity NOTE;
      assert ( i=4 and r=3.2 and b='1')
        report "***FAILED TEST: c12s02b03x00p01n02i03047 - Ports should conform to their subtype indication."
        severity ERROR;
      wait;
    END PROCESS TESTING;
  end block;

END c12s02b03x00p01n02i03047arch;
