
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
-- $Id: tc1738.vhd,v 1.2 2001-10-26 16:30:03 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s04b00x00p09n01i01738ent IS
END c09s04b00x00p09n01i01738ent;

ARCHITECTURE c09s04b00x00p09n01i01738arch OF c09s04b00x00p09n01i01738ent IS
  signal   s1 : bit;
BEGIN

  s1   <= not s1 after 70 ns;

  block_label1 : BLOCK (s1 = '1')
  begin
    assert not GUARD 
      report "PASSED TEST"
      severity NOTE;
  end block block_label1;
  
  TESTING: PROCESS(s1)
  BEGIN
    if (now = 70 ns) then
      assert FALSE 
        report "***PASSED TEST: c09s04b00x00p09n01i01738 - This test needs manual check, 'PASSED TEST' assertion should fire at 70 ns, 210 ns, 350 ns ...( the cycle is 140 ns)."
        severity NOTE;
    end if;
  END PROCESS TESTING;

END c09s04b00x00p09n01i01738arch;
