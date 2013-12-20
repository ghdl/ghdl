
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
-- $Id: tc2011.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p10n01i02011ent IS
END c07s02b02x00p10n01i02011ent;

ARCHITECTURE c07s02b02x00p10n01i02011arch OF c07s02b02x00p10n01i02011ent IS

  TYPE    int_vector is array (integer range <>) of INTEGER;
  SUBTYPE int_8 is int_vector(0 to 7);
  SUBTYPE int_4 is int_vector(0 to 3);

BEGIN
  TESTING: PROCESS
    CONSTANT slice_8a : int_8 := (1,2,3,4,5,6,7,8);
    VARIABLE slice_8b : int_8 := (1,2,3,4,5,6,7,8);
    VARIABLE target_1 : boolean;
    VARIABLE target_2 : boolean;
    VARIABLE target_3 : boolean;
    VARIABLE target_4 : boolean;
  BEGIN

    target_1 := slice_8a (3 to 3) < slice_8b (6 to 6);
    
    target_2 := slice_8a (3 to 3) <= slice_8b (7 to 7);
    
    target_3 := slice_8a (3 to 3) > slice_8b (2 to 2);
    
    target_4 := slice_8a (3 to 3) >= slice_8b (1 to 1);

    wait for 5 ns;
    assert NOT(   target_1 and
                  target_2 and
                  target_3 and
                  target_4 )
      report "***PASSED TEST: c07s02b02x00p10n01i02011" 
      severity NOTE;
    assert (   target_1 and
               target_2 and
               target_3 and
               target_4 )
      report "***FAILED TEST: c07s02b02x00p10n01i02011 - Ordering operators are loperable over the set of relational operations." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p10n01i02011arch;
