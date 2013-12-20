
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
-- $Id: tc1613.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s12b00x00p01n01i01613ent IS
END c08s12b00x00p01n01i01613ent;

ARCHITECTURE c08s12b00x00p01n01i01613arch OF c08s12b00x00p01n01i01613ent IS

  --
  -- Nested functions to test return statement.
  --
  function two  return integer is
    function one  return integer is
    begin
      return 1;
    end one;
  begin
    return one + one;
  end two;

BEGIN
  TESTING : PROCESS
  BEGIN
    assert NOT( two=2 )
      report "***PASSED TEST: c08s12b00x00p01n01i01613"
      severity NOTE;
    assert ( two=2 )
      report "***FAILED TEST: c08s12b00x00p01n01i01613 - Return statement applies to the innermost enclosing function."
      severity ERROR;
    wait;
  END PROCESS;

END c08s12b00x00p01n01i01613arch;
