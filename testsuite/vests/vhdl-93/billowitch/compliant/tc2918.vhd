
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
-- $Id: tc2918.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b01x02p06n01i02918ent IS
END c02s01b01x02p06n01i02918ent;

ARCHITECTURE c02s01b01x02p06n01i02918arch OF c02s01b01x02p06n01i02918ent IS

  function bit_func ( x : bit) return bit is
  begin
    return x;
  end bit_func ;
  function bit_vector_func ( x : bit_vector) return bit_vector is
  begin
    return x;
  end bit_vector_func ;
  function boolean_func ( x : boolean) return boolean is
  begin
    return x;
  end boolean_func ;
  function character_func ( x : character) return character is   
  begin
    return x;
  end character_func ;
  function integer_func ( x : integer) return integer is
  begin
    return x;
  end integer_func ;
  function real_func ( x : real) return real is
  begin
    return x;
  end real_func ;
  function string_func ( x : string) return string is  
  begin
    return x;
  end string_func ;
  function time_func ( x : time) return time is
  begin
    return x;
  end time_func ;

BEGIN
  TESTING: PROCESS
    variable v : bit_vector (1 to 3) ;
  BEGIN
    v(1) := '0';
    v(2) := '1';
    v(3) := '0';
    assert NOT(   (bit_func('1') = '1')       and
                  (bit_vector_func(v) = v)    and
                  (boolean_func(true) = true)    and
                  (character_func('X') = 'X')    and
                  (integer_func(6) = 6)       and
                  (real_func(3.14159) = 3.14159)    and
                  (string_func("qwertyuiop") = "qwertyuiop") and
                  (time_func(2 ns) = 2 ns)) 
      report "***PASSED TEST: c02s01b01x02p06n01i02918" 
      severity NOTE;
    assert (   (bit_func('1') = '1')       and
               (bit_vector_func(v) = v)    and
               (boolean_func(true) = true)    and
               (character_func('X') = 'X')    and
               (integer_func(6) = 6)       and
               (real_func(3.14159) = 3.14159)    and
               (string_func("qwertyuiop") = "qwertyuiop") and
               (time_func(2 ns) = 2 ns)) 
      report "***FAILED TEST: c02s01b01x02p06n01i02918 - Static signal as actual test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b01x02p06n01i02918arch;
