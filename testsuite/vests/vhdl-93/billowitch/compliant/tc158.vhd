
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
-- $Id: tc158.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c04s03b02x02p19n01i00158pkg is
  type rec_type is
    record
      a, b, c : integer;
    end record;
  procedure P1 (p : in rec_type; q: in integer; r: out integer);
end c04s03b02x02p19n01i00158pkg;

package body c04s03b02x02p19n01i00158pkg is
  procedure P1 (p : in rec_type; q: in integer; r: out integer) is
  begin
  end P1;
end c04s03b02x02p19n01i00158pkg;

use work.c04s03b02x02p19n01i00158pkg.all;
ENTITY c04s03b02x02p19n01i00158ent IS
END c04s03b02x02p19n01i00158ent;

ARCHITECTURE c04s03b02x02p19n01i00158arch OF c04s03b02x02p19n01i00158ent IS

BEGIN
  TESTING: PROCESS
    variable x : integer := 1;
  BEGIN
    P1 ((a => 1, b => 2, c => 3),      q => 10, r => x);  -- No_failure_here
    P1 (p => (a => 1, b => 2, c => 3), q => 10, r => x);  -- No_failure_here
    P1 (p.a => 1, p.b => 2, p.c => 3,  q => 10, r => x);  -- No_failure_here
    P1 (p => (1, 2, 3),                q => 10, r => x);  -- No_failure_here
    assert FALSE
      report "***PASSED TEST: c04s03b02x02p19n01i00158"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p19n01i00158arch;
