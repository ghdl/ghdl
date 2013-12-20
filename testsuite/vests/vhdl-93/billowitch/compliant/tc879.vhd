
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
-- $Id: tc879.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s01b00x00p04n01i00879pkg_a is
  -- define a subtype to be used elsewhere
  subtype EIGHTIES is INTEGER range 1980 to 1989;
end c10s01b00x00p04n01i00879pkg_a;

package c10s01b00x00p04n01i00879pkg_b is
  use WORK.c10s01b00x00p04n01i00879pkg_a.EIGHTIES;
  function INTO_EIGHTIES ( ARG: in INTEGER ) return EIGHTIES;
end c10s01b00x00p04n01i00879pkg_b;

package body c10s01b00x00p04n01i00879pkg_b is
  -- map any integer into range 1980 : 1989 based on one's digit
  function INTO_EIGHTIES ( ARG: in INTEGER ) return EIGHTIES is
    variable RETVAL: EIGHTIES;
  begin
    RETVAL := ( ( abs ARG ) mod 10 ) + 1980;
    return RETVAL;
  end INTO_EIGHTIES;
end c10s01b00x00p04n01i00879pkg_b;


ENTITY c10s01b00x00p04n01i00879ent IS
END c10s01b00x00p04n01i00879ent;

use WORK.c10s01b00x00p04n01i00879pkg_a.all;
use WORK.c10s01b00x00p04n01i00879pkg_b.all;
ARCHITECTURE c10s01b00x00p04n01i00879arch OF c10s01b00x00p04n01i00879ent IS
  signal THE_INPUT   : INTEGER;
  signal THE_OUTPUT   : INTEGER;
BEGIN
  TESTING: PROCESS
    variable   k : integer := 0;
  BEGIN
    for I in 120 to 149 loop
      THE_INPUT    <= I;
      THE_OUTPUT    <= INTO_EIGHTIES( I );
      wait for 1 ns;
      if ( THE_OUTPUT < 1980 or THE_OUTPUT > 1989 ) then
        k := 1;
      end if;
      assert ( ( THE_OUTPUT >= 1980 ) and ( THE_OUTPUT <= 1989 ) )
        report "output is out of range" 
        severity FAILURE;
    end loop;
    assert NOT( k=0 )
      report "***PASSED TEST: c10s01b00x00p04n01i00879"
      severity NOTE;
    assert ( k=0 )
      report "***FAILED TEST: c10s01b00x00p04n01i00879 - Declaration is formed by the subprogram declaration together with the corresponding subprogram body."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c10s01b00x00p04n01i00879arch;
