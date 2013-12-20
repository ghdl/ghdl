
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
-- $Id: tc969.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p05n01i00969ent IS
END c06s03b00x00p05n01i00969ent;

ARCHITECTURE c06s03b00x00p05n01i00969arch OF c06s03b00x00p05n01i00969ent IS

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
    S1.t := 11 ns;
    S1.u := 'A';
    S1.v := 2.1;
    S1.w := NOTE;
    S1.x := '0' ;   -- legal.
    S1.y := 12  ;
    S1.z := true;
    S2.t := S1.t;
    S2.u := S1.u;
    S2.v := S1.v;
    S2.w := S1.w;
    S2.x := S1.x;
    S2.y := S1.y;
    S2.z := S1.z;
    assert NOT(   
      S1.t = 11 ns   and
      S1.u = 'A'   and
      S1.v = 2.1   and
      S1.w = NOTE   and
      S1.x = '0'    and
      S1.y = 12     and
      S1.z = true   and
      S2.t = 11 ns   and
      S2.u = 'A'   and
      S2.v = 2.1   and
      S2.w = NOTE   and
      S2.x = '0'    and
      S2.y = 12     and
      S2.z = true   )
      report "***PASSED TEST: c06s03b00x00p05n01i00969" 
      severity NOTE;
    assert (   
      S1.t = 11 ns   and
      S1.u = 'A'   and
      S1.v = 2.1   and
      S1.w = NOTE   and
      S1.x = '0'    and
      S1.y = 12     and
      S1.z = true   and
      S2.t = 11 ns   and
      S2.u = 'A'   and
      S2.v = 2.1   and
      S2.w = NOTE   and
      S2.x = '0'    and
      S2.y = 12     and
      S2.z = true   )
      report "***FAILED TEST: c06s03b00x00p05n01i00969 - The prefix is not appropriate for the type of the object or value denoted by the suffix." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p05n01i00969arch;
