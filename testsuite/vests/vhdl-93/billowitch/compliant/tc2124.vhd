
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
-- $Id: tc2124.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b04x00p20n01i02124ent IS
END c07s02b04x00p20n01i02124ent;

ARCHITECTURE c07s02b04x00p20n01i02124arch OF c07s02b04x00p20n01i02124ent IS

  procedure CheckConcat(
    result       : STRING;
    reference    : STRING;
    left, right    : INTEGER
    ) is
    variable match : BOOLEAN;
  begin
    if result'LENGTH /= reference'LENGTH then
      assert FALSE report "FAIL: length does not match";
    elsif result'LEFT /= left then
      assert FALSE report "FAIL: 'LEFT is wrong";
    elsif result'RIGHT /= right then
      assert FALSE report "FAIL: 'RIGHT is wrong";
    elsif result /= reference then
      assert FALSE report "FAIL: value is wrong";
    else
      assert result = reference report "FAIL: value is wrong";
    end if;
    assert NOT(   result'LENGTH = reference'LENGTH   and
                  result'LEFT = left         and
                  result'RIGHT = right         and
                  result = reference         )
      report "***PASSED TEST: c07s02b04x00p20n01i02124"
      severity NOTE;
    assert (   result'LENGTH = reference'LENGTH   and
               result'LEFT = left         and
               result'RIGHT = right         and
               result = reference         )
      report "***FAILED TEST: c07s02b04x00p20n01i02124 - Concatenation of string in function call test failed."
      severity ERROR;
  end;

BEGIN
  TESTING : PROCESS
    subtype String3to3 is STRING(3 to 3);
  BEGIN
    --  VHDL87: CheckConcat(String3to3'("9") & "A", "9A", 3, 4);
    CheckConcat(String3to3'("9") & "A", "9A", 1, 2);  -- VHDL93
    wait;
  END PROCESS TESTING;

END c07s02b04x00p20n01i02124arch;
