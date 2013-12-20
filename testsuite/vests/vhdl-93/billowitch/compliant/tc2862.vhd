
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
-- $Id: tc2862.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s10b00x00p04n03i02862ent IS
END c13s10b00x00p04n03i02862ent;

ARCHITECTURE c13s10b00x00p04n03i02862arch OF c13s10b00x00p04n03i02862ent IS
  constant   one   : bit_vector := X"FF";
  constant   two   : bit_vector := X%FF%;
  constant   three   : bit_vector := o"77";
  constant   four   : bit_vector := o%77%;
  constant   five   : bit_vector := b"1111_1111";
  constant   six   : bit_vector := b%1111_1111%;

BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT(    one=two    and 
                   three=four    and 
                   five=six    )
      report "***PASSED TEST: c13s10b00x00p04n03i02862"
      severity NOTE;
    assert (    one=two    and 
                three=four    and 
                five=six    )
      report "***FAILED TEST: c13s10b00x00p04n03i02862 - Percent character (%) can replace the quotation character ("") in bit string literals."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s10b00x00p04n03i02862arch;
