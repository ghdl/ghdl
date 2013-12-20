
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
-- $Id: tc1040.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s04b00x00p03n01i01040ent IS
END c06s04b00x00p03n01i01040ent;

ARCHITECTURE c06s04b00x00p03n01i01040arch OF c06s04b00x00p03n01i01040ent IS
  type A is array (1 to 10) of integer;
  function foo (f:integer :=  3) return A is
    variable v: A := (1,2,3,4,5,6,7,8,9,10);
  begin
    return v;
  end foo;
BEGIN
  TESTING: PROCESS
    variable k : integer := 0;
  BEGIN
    k := foo(3)(3);
    assert NOT( k=3 )
      report "***PASSED TEST: c06s04b00x00p03n01i01040"
      severity NOTE;
    assert ( k=3 )
      report "***FAILED TEST: c06s04b00x00p03n01i01040 - The prefix of an indexed name must be appropriate for an array type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p03n01i01040arch;
