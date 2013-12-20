
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
-- $Id: tc2980.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s04b00x00p03n02i02980ent IS
END c02s04b00x00p03n02i02980ent;

ARCHITECTURE c02s04b00x00p03n02i02980arch OF c02s04b00x00p03n02i02980ent IS
  -- Create low-level resolution function and its subtypes.
  function Always_Zero( S : BIT_VECTOR ) return BIT is
  begin
    return( '0' );
  end Always_Zero;

  subtype BIT_SUB is Always_Zero BIT;
  type NEW_BIT_VECTOR is array( 1 to 10 ) of BIT_SUB;
  
  -- Create the composite signal resolved at both levels.
  signal ONE   : NEW_BIT_VECTOR;
BEGIN
  
  -- Create two drivers for the composite.
  ONE  <= NEW_BIT_VECTOR'(B"1111111111") after 10 ns;
  ONE  <= NEW_BIT_VECTOR'(B"0000000000") after 20 ns;
  
  TESTING: PROCESS(one)
  BEGIN
    assert NOT( ONE = B"0000000000" )
      report "***PASSED TEST: c02s04b00x00p03n02i02980"
      severity NOTE;
    assert ( ONE = B"0000000000" )
      report "***FAILED TEST: c02s04b00x00p03n02i02980 - Low level resolution function does not got called."
      severity ERROR;
  END PROCESS TESTING;

END c02s04b00x00p03n02i02980arch;
