
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
-- $Id: tc159.vhd,v 1.2 2001-10-26 16:30:11 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c04s03b02x02p19n05i00159pkg is
  type       t is array (1 to 4) of integer;
  procedure    p (a: in t);
end c04s03b02x02p19n05i00159pkg;

package body c04s03b02x02p19n05i00159pkg is
  procedure p (a: in t) is
  begin
  end p;
end c04s03b02x02p19n05i00159pkg;


use work.c04s03b02x02p19n05i00159pkg.all;
ENTITY c04s03b02x02p19n05i00159ent IS
END c04s03b02x02p19n05i00159ent;

ARCHITECTURE c04s03b02x02p19n05i00159arch OF c04s03b02x02p19n05i00159ent IS

BEGIN
  TESTING: PROCESS
    variable i : integer := 2;
  BEGIN
    p (t'(i => 12, others => 0));  -- Failure_here
    assert FALSE
      report "***FAILED TEST: c04s03b02x02p19n05i00159 - Subelements of an association list may only be locally static names."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p19n05i00159arch;
