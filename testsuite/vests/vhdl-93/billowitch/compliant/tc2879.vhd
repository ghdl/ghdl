
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
-- $Id: tc2879.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b00x00p07n01i02879ent IS
  function func1(constant flag:in integer) return integer;
  
  function func1(constant flag:in integer) return integer is
    variable v1 : integer;
  begin
    if (flag = 0) then
      return 0;
    else
      return ((func1(flag-1)) + 1);
    end if;
  end func1;
END c02s01b00x00p07n01i02879ent;

ARCHITECTURE c02s01b00x00p07n01i02879arch OF c02s01b00x00p07n01i02879ent IS

BEGIN
  TESTING: PROCESS
    variable x:integer;
  BEGIN
    x:=99;
    assert (x=99) report "Initialization of integer variables incorrect"
      severity failure;
    x:= func1(3);
    assert NOT( x=3 )
      report "***PASSED TEST: c02s01b00x00p07n01i02879"
      severity NOTE;
    assert ( x=3 )
      report "***FAILED TEST: c02s01b00x00p07n01i02879 - Functions resursion call test incorrect."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b00x00p07n01i02879arch;
