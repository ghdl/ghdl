
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
-- $Id: tc1459.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s07b00x00p04n01i01459ent IS
END c08s07b00x00p04n01i01459ent;

ARCHITECTURE c08s07b00x00p04n01i01459arch OF c08s07b00x00p04n01i01459ent IS

begin
  transmit: process
    variable delay : integer := 1;
    variable m     : integer := 0;
    variable n     : integer := 0;
  begin
    if    delay = 0 then
      m := 1;
    elsif delay = 4 then
      n := 1;
    end if;
    assert NOT((m = 0) and (n = 0))
      report "***PASSED TEST: c08s07b00x00p04n01i01459"
      severity NOTE;
    assert (m = 0) and (n = 0)
      report "***FAILED TEST: c08s07b00x00p04n01i01459 - all conditions should be evaluated and yield FALSE" 
      severity ERROR;
    wait;
  end process transmit;

END c08s07b00x00p04n01i01459arch;
