
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
-- $Id: tc1677.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s01b00x00p08n01i01677ent IS
END c09s01b00x00p08n01i01677ent;

ARCHITECTURE c09s01b00x00p08n01i01677arch OF c09s01b00x00p08n01i01677ent IS

  SUBTYPE    bit_vector_4 is bit_vector ( 0 to 3 );
  SUBTYPE    bit_vector_8 is bit_vector ( 0 to 7 );
  SIGNAL     v_slice : bit_vector_8 := B"1010_1100";

BEGIN

  labeled : block
    port ( v : OUT bit_vector_4 := "1010");
    port map ( v_slice ( 0 to 3 ));
  begin
    v <= B"0101" after 10 ns; -- only driver created ..
  end block;

  TESTING: PROCESS
  BEGIN

    assert (v_slice = B"1010_1100")
      report "Condition error: value of signal V_SLICE incorrect"
      severity failure;

    wait for 10 ns;

    assert NOT(v_slice = B"0101_1100")
      report "***PASSED TEST: c09s01b00x00p08n01i01677"
      severity NOTE;
    assert (v_slice = B"0101_1100")
      report "***FAILED TEST: c09s01b00x00p08n01i01677 - The value of signal V_SLICE was not properly updated."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c09s01b00x00p08n01i01677arch;
