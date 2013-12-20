
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
-- $Id: tc3066.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s04b02x00p02n01i03066ent IS
END c12s04b02x00p02n01i03066ent;

ARCHITECTURE c12s04b02x00p02n01i03066arch OF c12s04b02x00p02n01i03066ent IS
  signal    V :    BIT_VECTOR(1 to 4);
BEGIN
  FG1: for i in V'range generate
    B: block
    begin
      V(i) <= '0', '1' after i * 10 ns;
      -- signals should get different delays
    end block;
  end generate;

  TESTING: PROCESS(V)
    variable ok : integer := 1;
  BEGIN
    if (Now = 10 ns) then
      if not(V(1)'event and V(1) = '1') then
        ok := 0;
      end if;
    elsif (Now = 20 ns) then
      if not(V(2)'event and V(2) = '1') then
        ok := 0;
      end if;
    elsif (Now = 30 ns) then
      if not(V(3)'event and V(3) = '1') then
        ok := 0;
      end if;
    end if;
    if (Now > 30 ns) then
      assert NOT( ok = 1 )
        report "***PASSED TEST: c12s04b02x00p02n01i03066"
        severity NOTE;
      assert ( ok = 1 )
        report "***FAILED TEST: c12s04b02x00p02n01i03066 - Generate statement semantic test failed."
        severity ERROR;
    end if;
  END PROCESS TESTING;

END c12s04b02x00p02n01i03066arch;
