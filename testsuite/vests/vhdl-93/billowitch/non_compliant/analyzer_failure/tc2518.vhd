
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
-- $Id: tc2518.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b05x00p03n03i02518ent IS
END c07s03b05x00p03n03i02518ent;

ARCHITECTURE c07s03b05x00p03n03i02518arch OF c07s03b05x00p03n03i02518ent IS

BEGIN
  TESTING: PROCESS
    type Grapes is array (1 to 4) of real;
    variable Green : Grapes;
  BEGIN
    Green := Grapes (1.1, 1.2, 1.3, 1.4);
                                        -- Failure_here
                                        -- Aggregate is not allowed.
    assert FALSE 
      report "***FAILED TEST: c07s03b05x00p03n03i02518 - Operand cannot be the literal null, an alloator, an aggregate, or a string literal."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b05x00p03n03i02518arch;
