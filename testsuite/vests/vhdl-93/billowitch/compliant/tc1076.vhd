
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
-- $Id: tc1076.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p01n01i01076ent IS
  subtype    line      is integer range 0 to 15;
  subtype    cmd      is integer range 0 to 3;
  type    d_lines   is array (line range <>) of bit;
  subtype    data_line is d_lines(line);
  subtype    cmd_line  is d_lines(cmd);
END c06s05b00x00p01n01i01076ent;

ARCHITECTURE c06s05b00x00p01n01i01076arch OF c06s05b00x00p01n01i01076ent IS

BEGIN
  TESTING: PROCESS
    variable d1    : data_line := (0 to 3 => '1', others => '0');
    variable instr    : cmd_line;
  BEGIN
    --
    -- Test assigning a slice to a full array
    --
    instr := d1(0 to 3);
    for i in 0 to 3 loop
      assert instr(i) = '1'
        report "Slice to full array assignment failed."
        severity note ;
    end loop;

    --
    -- Now try a full array to a slice
    --
    d1(8 to 11) := instr;
    for i in 8 to 11 loop
      assert d1(i) = '1'
        report "Full array to slice assignment failed."
        severity note ;
    end loop;

    --
    -- Now try assigning a slice to a slice
    --
    d1(8 to 11) := d1(4 to 7);
    for i in 4 to 15 loop
      assert d1(i) = '0'
        report "Slice to slice assignment failed."
        severity note ;
    end loop;

    assert NOT( instr = "1111" and d1 = "1111000000000000" )
      report "***PASSED TEST: c06s05b00x00p01n01i01076"
      severity NOTE;
    assert ( instr = "1111" and d1 = "1111000000000000" )
      report "***FAILED TEST: c06s05b00x00p01n01i01076 - A slice name denotes a one-dimensional array composed of a sequence of consecutive elements of another one-dimensional array."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p01n01i01076arch;
