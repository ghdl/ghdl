
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
-- $Id: tc1749.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b00x00p16n01i01749ent IS
END c09s05b00x00p16n01i01749ent;

ARCHITECTURE c09s05b00x00p16n01i01749arch OF c09s05b00x00p16n01i01749ent IS
  signal a      : bit;
  signal b      : bit;
  signal grd    : boolean;
BEGIN
  grd   <=   not grd after 75 ns;

  block_label : BLOCK (grd)
  begin
    b <= guarded '1' after 1 ns;
  end block block_label;

  BG: block (grd)
  begin
    TESTING: PROCESS
    BEGIN
      if GUARD then
        a <= '1' after 1 ns;
      end if;
      wait on GUARD, a;
    END PROCESS TESTING;
  end block BG;

  process(a,b)
  begin
    if (now > 1 ns) then
      assert NOT( a=b )
        report "***PASSED TEST: c09s05b00x00p16n01i01749"
        severity NOTE;
      assert ( a=b )
        report "***FAILED TEST: c09s05b00x00p16n01i01749 - Concurrent signal assignment test failed."
        severity ERROR;
    end if;
  end process;

END c09s05b00x00p16n01i01749arch;
