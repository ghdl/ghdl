
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
-- $Id: tc3055.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c12s03b01x00p02n03i03055pkg is
  subtype BYTE is BIT_VECTOR(7 downto 0);
  function BIN_TO_INTG (IN_DATA : BYTE) return INTEGER;
end c12s03b01x00p02n03i03055pkg;

package body c12s03b01x00p02n03i03055pkg is
  function BIN_TO_INTG (IN_DATA : BYTE) return INTEGER is
    variable SUM : INTEGER := 0;
  begin
    for I in 7 downto 0 loop
      if (IN_DATA(I) = '1') then
        SUM := SUM + (2**I);
      end if;
    end loop;
    return SUM;
  end BIN_TO_INTG;
end c12s03b01x00p02n03i03055pkg;

use WORK.c12s03b01x00p02n03i03055pkg.all;
ENTITY c12s03b01x00p02n03i03055ent IS
END c12s03b01x00p02n03i03055ent;

ARCHITECTURE c12s03b01x00p02n03i03055arch OF c12s03b01x00p02n03i03055ent IS

BEGIN
  TESTING: PROCESS
    variable S1 : BYTE := "00001111";
    variable X  : INTEGER;
  BEGIN
    X := BIN_TO_INTG(S1) ;
    assert NOT(X = 15)
      report "***PASSED TEST: c12s03b01x00p02n03i03055" 
      severity NOTE;
    assert (X = 15)
      report "***FAILED TEST: c12s03b01x00p02n03i03055 - Subprogram Body should be elaaborated before subprogram call."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s03b01x00p02n03i03055arch;
