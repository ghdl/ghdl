
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
-- $Id: tc968.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p05n01i00968ent IS
END c06s03b00x00p05n01i00968ent;

ARCHITECTURE c06s03b00x00p05n01i00968arch OF c06s03b00x00p05n01i00968ent IS

BEGIN
  TESTING: PROCESS
    type rec_type is
      record
        t : time;
        u : character;
        v : real;
        w : severity_level;
        x : bit;
        y : integer;
        z : boolean;
      end record;
    variable S1, S2 :rec_type;
  BEGIN
    S1.t := 10 ns;
    S1.u := 'A';
    S1.v := 1.2;
    S1.w := ERROR;
    S1.y := 12  ;
    S1.x := '0' ;   -- legal.
    S2   := S1  ;
    assert NOT(S2.t=10 ns and S2.u='A' and S2.v=1.2 and S2.w=ERROR and S2.x='0' and S2.y=12 and S2.z=false) 
      report "***PASSED TEST: c06s03b00x00p05n01i00968" 
      severity NOTE;
    assert (S2.t=10 ns and S2.u='A' and S2.v=1.2 and S2.w=ERROR and S2.x='0' and S2.y=12 and S2.z=false) 
      report "***FAILED TEST: c06s03b00x00p05n01i00968 - Suffix should denote an element of a record object or value."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p05n01i00968arch;
