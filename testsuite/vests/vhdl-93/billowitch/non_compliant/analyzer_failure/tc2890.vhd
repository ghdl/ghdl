
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
-- $Id: tc2890.vhd,v 1.2 2001-10-26 16:30:23 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b01x00p05n03i02890ent IS
END c02s01b01x00p05n03i02890ent;

ARCHITECTURE c02s01b01x00p05n03i02890arch OF c02s01b01x00p05n03i02890ent IS
  function F1 ( A,B : integer) return integer;
  function F1 ( A,B : integer ) return integer is
  begin
    A := 2 ;  -- Failure_here
    --ERROR: formal paramters not explicitly given are constant and therfore
    -- this assignment is illegal.

    B := B * A;  -- Failure_here
    --ERROR: formal paramters not explicitly given are constant and therfore
    -- this assignment is illegal.
    
    return 3;
  end F1;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c02s01b01x00p05n03i02890 - Cannot assign a value to a 'constant'."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b01x00p05n03i02890arch;
