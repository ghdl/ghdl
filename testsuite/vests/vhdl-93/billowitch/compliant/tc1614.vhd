
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
-- $Id: tc1614.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s12b00x00p01n01i01614ent IS
END c08s12b00x00p01n01i01614ent;

ARCHITECTURE c08s12b00x00p01n01i01614arch OF c08s12b00x00p01n01i01614ent IS

  --
  -- Nested procedures to test return statement.
  --
  procedure two ( variable val : inout integer ) is
    procedure one ( variable val : out integer ) is
    begin
      val := 1;
      return;
      val := 2;                     -- should never get here
    end one;
  begin
    one(val);
    val := val * 2;
    return;
    val := val * 2;                   -- should never get here
  end two;

BEGIN
  TESTING : PROCESS
    variable v1 : integer;
  BEGIN
    two (v1);
    assert NOT( v1=2 )
      report "***PASSED TEST: c08s12b00x00p01n01i01614"
      severity NOTE;
    assert ( v1=2 )
      report "***FAILED TEST: c08s12b00x00p01n01i01614 - Return statement applies to the innermost enclosing function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s12b00x00p01n01i01614arch;
