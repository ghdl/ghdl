
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
-- $Id: tc2511.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b04x00p03n03i02511ent IS
END c07s03b04x00p03n03i02511ent;

ARCHITECTURE c07s03b04x00p03n03i02511arch OF c07s03b04x00p03n03i02511ent IS

BEGIN
  TESTING: PROCESS
    function f1 return integer is
    begin 
      return 2; 
    end;
    variable k : integer := 0;
  BEGIN
    k := integer'(f1/f1);
    assert NOT( k=1 )
      report "***PASSED TEST: c07s03b04x00p03n03i02511"
      severity NOTE;
    assert ( k=1 )
      report "***FAILED TEST: c07s03b04x00p03n03i02511 - The evaluation of a qualified expression evaluates the operand and checks that its value belongs to the subtype denoted by the type mark."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b04x00p03n03i02511arch;
