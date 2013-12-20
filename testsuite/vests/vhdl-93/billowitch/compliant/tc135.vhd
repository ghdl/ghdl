
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
-- $Id: tc135.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p08n01i00135ent IS
END c04s03b02x02p08n01i00135ent;

ARCHITECTURE c04s03b02x02p08n01i00135arch OF c04s03b02x02p08n01i00135ent IS
  type    AT1 is array (INTEGER range <>, INTEGER range <>) of INTEGER;
  subtype ST1 is AT1(1 to 2, 1 to 2);
BEGIN
  TESTING: PROCESS

    procedure Proc1(P : inout ST1; ref : in ST1; set : in ST1) is
    begin
      if (P=ref) then
        P := set;
      end if;
    end;
    
    variable V : ST1 := ((1, 2), (3, 4));

  BEGIN
    V := ((1, 2), (3, 4));
    Proc1(  P(1,1) => V(2,2), P(1,2) => V(2,1),
            P(2,1) => V(1,2), P(2,2) => V(1,1),
            ref => ((4, 3), (2, 1)), set => ((9, 8), (7, 6)));  -- test here
    assert V = ((6, 7), (8, 9)) report "FAIL: actual V didn't get set right";

    assert NOT( V = ((6,7),(8,9)) )
      report "***PASSED TEST: c04s03b02x02p08n01i00135"
      severity NOTE;
    assert ( V = ((6,7),(8,9)) )
      report "***FAILED TEST: c04s03b02x02p08n01i00135 - Association element in an association list test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p08n01i00135arch;
