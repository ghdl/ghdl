
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
-- $Id: tc880.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s01b00x00p05n01i00880pkg_1 is
  subtype LOWERCASE is CHARACTER range 'a' to 'z';
end c10s01b00x00p05n01i00880pkg_1;

use WORK.c10s01b00x00p05n01i00880pkg_1.LOWERCASE;
package c10s01b00x00p05n01i00880pkg_2 is
  function ISLOWER ( TESTCHAR: in CHARACTER ) return BOOLEAN;
end c10s01b00x00p05n01i00880pkg_2;

package body c10s01b00x00p05n01i00880pkg_2 is
  function ISLOWER ( TESTCHAR: in CHARACTER ) return BOOLEAN is
  begin
    if ( ( TESTCHAR >= LOWERCASE'LOW ) and ( TESTCHAR <= LOWERCASE'HIGH )) then
      return TRUE;
    else
      return FALSE;
    end if;
  end ISLOWER;
end c10s01b00x00p05n01i00880pkg_2;

ENTITY c10s01b00x00p05n01i00880ent IS
END c10s01b00x00p05n01i00880ent;

-- run through all values of character
-- and post high if lowercase, low otherwise.  also, if is lowercase,
-- place value on small_letter.
use WORK.c10s01b00x00p05n01i00880pkg_1.LOWERCASE;
use WORK.c10s01b00x00p05n01i00880pkg_2.all;
ARCHITECTURE c10s01b00x00p05n01i00880arch OF c10s01b00x00p05n01i00880ent IS
  signal LOWER_TRUTH : BIT := '0';
  signal SMALL_LETTER: LOWERCASE;
  signal TEST_LETTER : CHARACTER;
BEGIN
  TESTING: PROCESS
    variable k : integer := 0;
  BEGIN

    for CHAR_AT_HAND in CHARACTER'LOW to CHARACTER'HIGH loop
      -- do the work
      TEST_LETTER    <= CHAR_AT_HAND;
      if ISLOWER( CHAR_AT_HAND ) then
        LOWER_TRUTH    <= '1';
        SMALL_LETTER    <= CHAR_AT_HAND;
      else
        LOWER_TRUTH    <= '0';
      end if;
      wait for 1 ns;
      -- make sure it happened
      if ( ( CHAR_AT_HAND >= LOWERCASE'LOW ) and ( CHAR_AT_HAND <= LOWERCASE'HIGH ) ) then
        if (ISLOWER(CHAR_AT_HAND) = false) then
          k := 1;
        end if;
        assert ( ISLOWER( CHAR_AT_HAND ) )
          report "ISLOWER is wrong"
          severity FAILURE;
        if (LOWER_TRUTH /= '1') then
          k := 1;
        end if;
        assert ( LOWER_TRUTH = '1' )
          report "LOWER_TRUTH is wrong"
          severity FAILURE;
        if (CHAR_AT_HAND /= SMALL_LETTER) then
          k := 1;
        end if;
        assert ( CHAR_AT_HAND = SMALL_LETTER )
          report "SMALL_LETTER is wrong"
          severity FAILURE;
      else
        if (LOWER_TRUTH /= '0') then
          k := 1;
        end if;
        assert ( LOWER_TRUTH = '0' )
          report "LOWER_TRUTH is wrong"
          severity FAILURE;
      end if;
    end loop;

    assert NOT( k=0 )
      report "***PASSED TEST: c10s01b00x00p05n01i00880"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c10s01b00x00p05n01i00880 - A declaration region is formed by a package declaration together with the corresponding body."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s01b00x00p05n01i00880arch;
