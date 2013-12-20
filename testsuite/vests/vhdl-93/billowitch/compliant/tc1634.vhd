
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
-- $Id: tc1634.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s12b00x00p05n01i01634ent IS
END c08s12b00x00p05n01i01634ent;

ARCHITECTURE c08s12b00x00p05n01i01634arch OF c08s12b00x00p05n01i01634ent IS

BEGIN
  TESTING: PROCESS
    type E is (A,B,C,D);
    subtype E1 is E range C to D;
    function F return E is
      variable V : E1 := C;
    begin
      return V;
    end F;
    variable k : E := A; 
  BEGIN
    k := F;
    assert NOT(k = C)
      report "***PASSED TEST: c08s12b00x00p05n01i01634" 
      severity NOTE;
    assert (k = C) 
      report "***FAILED TEST: c08s12b00x00p05n01i01634 - The return type must be the same base tyep declared in the specification of the function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s12b00x00p05n01i01634arch;
