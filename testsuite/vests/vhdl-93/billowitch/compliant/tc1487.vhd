
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
-- $Id: tc1487.vhd,v 1.2 2001-10-26 16:29:41 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s08b00x00p04n03i01487ent IS
END c08s08b00x00p04n03i01487ent;

ARCHITECTURE c08s08b00x00p04n03i01487arch OF c08s08b00x00p04n03i01487ent IS

BEGIN
  TESTING: PROCESS
    type     t_enum1 is (en1, en2, en3, en4);
    variable m : t_enum1 := en1;
    variable k : integer := 0;
  BEGIN
    case m is
      when en1 | en2 =>   k := 5;
      when en3 | en4 =>   k := 4;
      when others    =>   NULL;
    end case;
    assert NOT( k = 5 )
      report "***PASSED TEST: c08s08b00x00p04n03i01487"
      severity NOTE;
    assert ( k = 5 )
      report "***FAILED TEST: c08s08b00x00p04n03i01487 - Each choice in a case statement alternative must be of the same type as the expression." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s08b00x00p04n03i01487arch;
