
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
-- $Id: tc76.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b01x02p10n04i00076ent IS
END c04s03b01x02p10n04i00076ent;

ARCHITECTURE c04s03b01x02p10n04i00076arch OF c04s03b01x02p10n04i00076ent IS
  function F (constant S : BIT_VECTOR) return bit;
  function F (constant S : BIT_VECTOR) return bit is
    variable res_bit : bit := bit'('0');
  begin
    for I in S'LOW to S'HIGH loop
      if S(I) = bit'('1') then
        res_bit := bit'('1');
        exit;
      end if;
    end loop;
    return res_bit;
  end;
  signal X : F bit;       -- X is a resolved signal.
  signal P,Q : bit := '1';
BEGIN
  TESTING: PROCESS(P)
  BEGIN
    X <= P;
  END PROCESS TESTING;

  TESTING1: PROCESS(Q)
  BEGIN
    X <= Q;      --NO_Failure Here
  END PROCESS TESTING1;

  TEST: PROCESS
  BEGIN
    wait for 10 ns;
    assert NOT(X='1') 
      report "***PASSED TEST: c04s03b01x02p10n04i00076" 
      severity NOTE;
    assert (X='1') 
      report "***FAILED TEST:c04s03b01x02p10n04i00076 - A signal with multiple source should be a resolved signal."
      severity ERROR;
    wait;
  END PROCESS TEST;

END c04s03b01x02p10n04i00076arch;

