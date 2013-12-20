
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
-- $Id: tc885.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s01b00x00p10n01i00885ent IS
END c10s01b00x00p10n01i00885ent;

ARCHITECTURE c10s01b00x00p10n01i00885arch OF c10s01b00x00p10n01i00885ent IS
  signal S: INTEGER := 356;
BEGIN
  TESTING: PROCESS
    constant I: INTEGER := 105; -- loop parameter has same name
    variable k: integer := 0;
  BEGIN
    -- assign process constant I to S
    S <= I;
    wait for 1 ns;
    assert ( S = 105 )
      report "constant not properly assigned to signal"
      severity FAILURE;
    -- loop parameter has same name as constant declared in process
    for I in 1 to 5 loop
      -- assign loop parameter I to S
      S <= I;
      wait for 1 ns;
      if ((S<1) or (S>5)) then
        k := 1;
      end if;
      assert ( ( S >= 1 ) and ( S <= 5 ) )
        report "loop parameter not properly assigned to signal"
        severity FAILURE;
    end loop;
    assert NOT( k=0 )
      report "***PASSED TEST: c10s01b00x00p10n01i00885"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c10s01b00x00p10n01i00885 - A declaration region is formed by the text of a loop statement."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s01b00x00p10n01i00885arch;
