
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
-- $Id: tc1147.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s05b00x00p05n03i01147ent IS
END c06s05b00x00p05n03i01147ent;

ARCHITECTURE c06s05b00x00p05n03i01147arch OF c06s05b00x00p05n03i01147ent IS
  SUBTYPE thirteen is INTEGER range 0 to 12;
BEGIN
  TESTING: PROCESS
    VARIABLE null_array    : bit_vector ( 1 to 0 ); -- OK, a nice clean null array
    VARIABLE slice       : bit_vector ( thirteen );
  BEGIN

    assert NOT(    null_array = slice (11 to 10)   and
                   null_array = slice (-1 to -5)   and
                   null_array = slice (15 to 14)  )
      report "***PASSED TEST: c06s05b00x00p05n03i01147"
      severity NOTE;
    assert ( null_array = slice (11 to 10)   and
             null_array = slice (-1 to -5)   and
             null_array = slice (15 to 14)  )
      report "***FAILED TEST: c06s05b00x00p05n03i01147 - The bounds of a null slice need not belong to the subtype of the index."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p05n03i01147arch;
