
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
-- $Id: tc1345.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b01x00p06n05i01345ent IS
END c08s04b01x00p06n05i01345ent;

ARCHITECTURE c08s04b01x00p06n05i01345arch OF c08s04b01x00p06n05i01345ent IS
  signal k : integer;
BEGIN
  TESTING: PROCESS
  BEGIN
    k <=   1 after 10 ns,
           2 after 20 ns,
           3 after 30 ns,
           2 after 20 ns,
           4 after 40 ns,
           5 after 50 ns;
    wait for 80 ns;
    assert FALSE 
      report "***FAILED TEST: c08s04b01x00p06n05i01345 - The sequence of new transactions must be in ascending order with respect to time."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b01x00p06n05i01345arch;
