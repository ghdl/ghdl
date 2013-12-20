
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
-- $Id: tc2482.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c07s03b03x00p01n01i02482pkg is
  function uno return natural;
  constant a_bit : bit_vector (uno to uno) := ( uno => '1' );
end c07s03b03x00p01n01i02482pkg;

package body c07s03b03x00p01n01i02482pkg is
  function uno return natural is
  begin
    return 1;
  end uno;
end c07s03b03x00p01n01i02482pkg;

ENTITY c07s03b03x00p01n01i02482ent IS
END c07s03b03x00p01n01i02482ent;

ARCHITECTURE c07s03b03x00p01n01i02482arch OF c07s03b03x00p01n01i02482ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    uno;
    assert FALSE
      report "***FAILED TEST: c07s03b03x00p01n01i02482 - Function body is not defined."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b03x00p01n01i02482arch;
