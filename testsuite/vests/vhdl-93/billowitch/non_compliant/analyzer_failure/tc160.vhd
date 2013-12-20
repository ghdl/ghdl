
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
-- $Id: tc160.vhd,v 1.2 2001-10-26 16:30:11 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c04s03b02x02p19n04i00160pkg is
  type rec_type is
    record
      a, b, c : integer;
    end record;
  procedure P1 (p : in rec_type; q: in integer; r: out integer);
end c04s03b02x02p19n04i00160pkg;

package body c04s03b02x02p19n04i00160pkg is
  procedure P1 (p : in rec_type; q: in integer; r: out integer) is
  begin
    r := (p.a + p.b + p.c)/3 * q;
  end;
end c04s03b02x02p19n04i00160pkg;


use work.c04s03b02x02p19n04i00160pkg.all;
ENTITY c04s03b02x02p19n04i00160ent IS
END c04s03b02x02p19n04i00160ent;

ARCHITECTURE c04s03b02x02p19n04i00160arch OF c04s03b02x02p19n04i00160ent IS

BEGIN
  TESTING: PROCESS
    variable x : integer := 1;
  BEGIN
    P1 (p.a => 1, p.b => 2, p.a => 3, p.c => 4, q => 12);
                                        -- Failure_here
                                        -- p.a named twice.
    assert FALSE
      report "***FAILED TEST: c04s03b02x02p19n04i00160 - Subelements of an association list may only be assigned once."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p19n04i00160arch;
