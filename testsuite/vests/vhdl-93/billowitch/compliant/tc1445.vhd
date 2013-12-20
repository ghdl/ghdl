
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
-- $Id: tc1445.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s07b00x00p02n01i01445ent IS
END c08s07b00x00p02n01i01445ent;

ARCHITECTURE c08s07b00x00p02n01i01445arch OF c08s07b00x00p02n01i01445ent IS

begin
  transmit: process
    variable k : integer := 10;
    variable m : integer := 6;
    variable n : time    := 0 ns;
  begin
    if m > 5 then
      n := now;
      wait for 5 ns;
      n := now - n;
    end if;
    assert NOT(n = 5 ns)
      report "***PASSED TEST: c08s07b00x00p02n01i01445"
      severity NOTE;
    assert (n = 5 ns)
      report "***FAILED TEST: c08s07b00x00p02n01i01445 - WAIT FOR statement to be sequence statements of IF statement" 
      severity ERROR;
    wait;
  end process;

END c08s07b00x00p02n01i01445arch;
