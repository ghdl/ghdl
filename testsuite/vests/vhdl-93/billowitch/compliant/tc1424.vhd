
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
-- $Id: tc1424.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s06b00x00p05n01i01424ent IS
END c08s06b00x00p05n01i01424ent;

ARCHITECTURE c08s06b00x00p05n01i01424arch OF c08s06b00x00p05n01i01424ent IS

  procedure copy_into ( variable dest : out integer;
                        variable src  : in  integer := 0   ) is
    --
    -- This procedure copies the value of the second argument
    -- into the first argument.
    --
  begin
    dest := src;
  end copy_into;

BEGIN
  TESTING : PROCESS
    variable   v1 : integer := 0;
  BEGIN

    --
    -- Try it with only one parameter
    --
    copy_into(v1);            -- v1 <- (0)
    assert NOT(v1 = 0)
      report "***PASSED TEST: c08s06b00x00p05n01i01424"
      severity NOTE;
    assert (v1 = 0)
      report "***FAILED TEST: c08s06b00x00p05n01i01424 - Procedure call without an actual parameter part is permitted." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s06b00x00p05n01i01424arch;
