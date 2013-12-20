
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
-- $Id: tc2550.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b05x00p08n01i02550ent IS
END c07s03b05x00p08n01i02550ent;

ARCHITECTURE c07s03b05x00p08n01i02550arch OF c07s03b05x00p08n01i02550ent IS

BEGIN
  TESTING: PROCESS
    type century is  array (1 to 5) of integer ;
    type millenia is array (5 downto 1) of integer;
    variable hundreds : century  := (1,1,1,1,1);
    variable thousand : millenia ;
  BEGIN
    thousand := millenia (hundreds);
    assert NOT(thousand = (1,1,1,1,1)) 
      report "***PASSED TEST: c07s03b05x00p08n01i02550" 
      severity NOTE;
    assert (thousand = (1,1,1,1,1)) 
      report "***FAILED TEST: c07s03b05x00p08n01i02550 - Operand and the target type should have the same index type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b05x00p08n01i02550arch;
