
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
-- $Id: tc1080.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p01n02i01080ent IS
END c06s05b00x00p01n02i01080ent;

ARCHITECTURE c06s05b00x00p01n02i01080arch OF c06s05b00x00p01n02i01080ent IS
  SUBTYPE    bit_vector_4 is bit_vector ( 0 to 3 );
  SUBTYPE    bit_vector_8 is bit_vector ( 0 to 7 );
  SIGNAL   result  : boolean;

  CONSTANT    v_slice : bit_vector_8 := B"1010_1100";   

  procedure subprogram ( constant v : in bit_vector_4; signal resultt : out boolean ) is
  begin
    if (v = "1010") then
      resultt <= true;
    else
      resultt <= false;
    end if;
  end ;

BEGIN
  TESTING: PROCESS
  BEGIN
    subprogram ( v_slice ( 0 to 3 ), result );
    wait for 11 ns;
    assert NOT(result = true)
      report "***PASSED TEST: c06s05b00x00p01n02i01080"
      severity NOTE;
    assert (result = true)
      report "***FAILED TEST: c06s05b00x00p01n02i01080 - A slice of a constant should still be a constant."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p01n02i01080arch;
