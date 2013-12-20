
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
-- $Id: tc204.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b00x00p07n02i00204ent IS
END c03s01b00x00p07n02i00204ent;

ARCHITECTURE c03s01b00x00p07n02i00204arch OF c03s01b00x00p07n02i00204ent IS

BEGIN
  TESTING: PROCESS
    variable k : integer := 0;
  BEGIN
    for I in 5 downto 50 loop
      k := k + 1;
    end loop;
    assert NOT( k=0 )
      report "***PASSED TEST: c03s01b00x00p07n02i00204"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c03s01b00x00p07n02i00204 - In the case of L downto R, if L < R then the range is a null range."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b00x00p07n02i00204arch;
