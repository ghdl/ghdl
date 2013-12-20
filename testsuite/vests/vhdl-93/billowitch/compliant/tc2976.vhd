
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
-- $Id: tc2976.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s03b01x00p05n01i02976ent IS
END c02s03b01x00p05n01i02976ent;

ARCHITECTURE c02s03b01x00p05n01i02976arch OF c02s03b01x00p05n01i02976ent IS
  subtype    si is integer range 1 to 4;
  constant    c1 :  si := 2;
  signal    s1 :  integer;

  function "=" (constant c1,c2 : in integer) return boolean is
  begin
    return false;
  end;

BEGIN
  with (c1) select
    s1 <= 1 after 5 ns when 1,
    2 after 5 ns when 2,
    3 after 5 ns when 3,
    4 after 5 ns when others;

  TESTING: PROCESS
  BEGIN
    wait for 10 ns;
    assert NOT( (s1<=2) and (s1>=2) )
      report "***PASSED TEST: c02s03b01x00p05n01i02976"
      severity NOTE;
    assert ( (s1<=2) and (s1>=2) )
      report "***FAILED TEST: c02s03b01x00p05n01i02976 - Error in use of overloaded equality operator."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s03b01x00p05n01i02976arch;
