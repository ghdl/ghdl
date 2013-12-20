
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
-- $Id: tc163.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c04s03b02x02p20n01i00163pkg is
  procedure P1 (p : in integer := 0; r: inout integer);
end c04s03b02x02p20n01i00163pkg;

package body c04s03b02x02p20n01i00163pkg is
  procedure P1 (p : in integer := 0; r: inout integer) is
  begin
    r := p / 3 ;
  end;
end c04s03b02x02p20n01i00163pkg;


use work.c04s03b02x02p20n01i00163pkg.all;
ENTITY c04s03b02x02p20n01i00163ent IS
END c04s03b02x02p20n01i00163ent;

ARCHITECTURE c04s03b02x02p20n01i00163arch OF c04s03b02x02p20n01i00163ent IS

BEGIN
  TESTING: PROCESS
    variable x : integer := 1;
  BEGIN
    P1 (r => x);  -- No_failure_here
    -- no association for p
    assert NOT( x=0 )
      report "***PASSED TEST: c04s03b02x02p20n01i00163"
      severity NOTE;
    assert ( x=0 )
      report "***FAILED TEST: c04s03b02x02p20n01i00163 - Defualt value in an association list test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p20n01i00163arch;
