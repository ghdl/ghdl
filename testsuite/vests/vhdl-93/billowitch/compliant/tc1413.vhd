
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
-- $Id: tc1413.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s05b01x00p01n01i01413ent IS
END c08s05b01x00p01n01i01413ent;

ARCHITECTURE c08s05b01x00p01n01i01413arch OF c08s05b01x00p01n01i01413ent IS

BEGIN
  TESTING: PROCESS
    function check (i : integer) return real is
    begin
      return (1.0);
    end;
    type rec_type is
      record
        x : integer;
        y : real;
        z : boolean;
        b : bit;
      end record;
    type array_type is array (1 to 10) of rec_type;
    variable   v1 : array_type;   
    constant    i  : integer := 20;
  BEGIN
    v1 (1).x := i;
    v1 (1).y := check(i);
    v1 (1).z := true;
    v1 (1).b := bit'('0');
    assert NOT(v1(1).x=20 and v1(1).y=1.0 and v1(1).z=true and v1(1).b='0') 
      report "***PASSED TEST: c08s05b01x00p01n01i01413" 
      severity NOTE;
    assert (v1(1).x=20 and v1(1).y=1.0 and v1(1).z=true and v1(1).b='0') 
      report "***FAILED TEST: c08s05b01x00p01n01i01413 Each element of the array variable there is a matching element on the right hand side." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s05b01x00p01n01i01413arch;
