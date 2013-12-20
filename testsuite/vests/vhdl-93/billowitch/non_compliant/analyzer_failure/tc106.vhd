
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
-- $Id: tc106.vhd,v 1.2 2001-10-26 16:30:05 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x00p29n06i00106ent IS
  port ( signal S : out bit) ;
END c04s03b02x00p29n06i00106ent;

ARCHITECTURE c04s03b02x00p29n06i00106arch OF c04s03b02x00p29n06i00106ent IS

BEGIN
  TESTING: PROCESS
    variable T : TIME := 10 ns;
  BEGIN
    if (S'LAST_EVENT = T) then  -- Failure_here
    end if;
    assert FALSE 
      report "***FAILED TEST: c04s03b02x00p29n06i00106 - The attribute LAST_EVENT of a signal of mode out cannot be read." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x00p29n06i00106arch;
