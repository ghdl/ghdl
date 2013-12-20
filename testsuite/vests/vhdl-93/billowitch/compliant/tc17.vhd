
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
-- $Id: tc17.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p08n01i00017ent IS
END c04s02b00x00p08n01i00017ent;

ARCHITECTURE c04s02b00x00p08n01i00017arch OF c04s02b00x00p08n01i00017ent IS

  -- Forward declaration of the function.
  function WIRED_OR( S : BIT_VECTOR ) return BIT;

  -- Declare the subtype.
  subtype RBIT is WIRED_OR BIT;

  -- Declare the actual function.
  function WIRED_OR( S : BIT_VECTOR ) return BIT is
  begin
    assert FALSE 
      report "***PASSED TEST: c04s02b00x00p08n01i00017" 
      severity NOTE;
    if  ( (S(0) = '1') OR (S(1) = '1')) then
      return '1';
    end if;
    return '0';
  end WIRED_OR;

  -- Declare a signal of that type.  A resolved signal.
  signal S : RBIT;

BEGIN

  -- A concurrent signal assignment.  Driver # 1.
  S <= '1';

  TESTING: PROCESS
  BEGIN
    -- Verify that resolution function getting called.
    S <= '1' after 10 ns;
    wait on S;
    assert NOT( S = '1' ) 
      report "***PASSED TEST: c04s02b00x00p08n01i00017"
      severity NOTE;
    assert ( S = '1' )
      report "***FAILED TEST: c04s02b00x00p08n01i00017 - If a resolution function name appears in a subtype, all signals declared to be of that subtype are resolved by that function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p08n01i00017arch;
