
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
-- $Id: tc3084.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s06b03x00p02n01i03084ent IS
END c12s06b03x00p02n01i03084ent;

ARCHITECTURE c12s06b03x00p02n01i03084arch OF c12s06b03x00p02n01i03084ent IS
  signal S1 : BIT;
BEGIN

  S1 <= transport '1' after 5 ns,
        '0' after 15 ns;

  A : block(S1 = '1')
  begin
    process
    begin
      wait on GUARD;
      if GUARD then
        assert false
          report "No failure; Changes on signal S1 have modified the GUARD signal" 
          severity NOTE;
      else
        assert false
          report "No failure; Changes on signal S1 have modified the GUARD signal"
          severity NOTE;
      end if;
    end process;
  end block A;

  TESTING: PROCESS
  BEGIN
    wait for 50 ns;
    assert FALSE
      report "***PASSED TEST: c12s06b03x00p02n01i03084 - This test needs manual check to see other two PASS assertion note."
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c12s06b03x00p02n01i03084arch;
