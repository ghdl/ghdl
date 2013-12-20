
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
-- $Id: tc1308.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s04b00x00p07n01i01308ent IS
END c08s04b00x00p07n01i01308ent;

ARCHITECTURE c08s04b00x00p07n01i01308arch OF c08s04b00x00p07n01i01308ent IS
  signal S : BIT;
  signal T : BIT;
BEGIN
  TESTING: PROCESS
  BEGIN
    (S,T) <= ('1','0') after 10 ns;
    wait for 1 ns;
    assert FALSE 
      report "***FAILED TEST: c08s04b00x00p07n01i01308 - If the target of the signal assignment statement is in the form of an aggregate, then the type of the aggregate must be determinable from the context."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s04b00x00p07n01i01308arch;
