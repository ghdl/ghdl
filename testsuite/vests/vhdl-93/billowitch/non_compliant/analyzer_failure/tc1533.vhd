
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
-- $Id: tc1533.vhd,v 1.2 2001-10-26 16:30:10 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s09b00x00p09n02i01533ent IS
END c08s09b00x00p09n02i01533ent;

ARCHITECTURE c08s09b00x00p09n02i01533arch OF c08s09b00x00p09n02i01533ent IS

BEGIN
  TESTING: PROCESS
    variable    i1, i2, i3 : integer := 1;
  BEGIN

    --
    -- Initialize two integer variables so their division yeilds a real
    -- 
    i2 := 11;
    i3 := 3;
    --
    -- Loop must have discrete parameters
    --
    L1: for c in i1 to real(i2) / i3 loop     -- parameters must be discrete
      null;
    end loop L1;

    assert FALSE 
      report "***FAILED TEST: c08s09b00x00p09n02i01533 - Loop parameters must be discrete."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p09n02i01533arch;
