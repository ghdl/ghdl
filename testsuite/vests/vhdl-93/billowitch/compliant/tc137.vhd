
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
-- $Id: tc137.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p08n01i00137ent IS
END c04s03b02x02p08n01i00137ent;

ARCHITECTURE c04s03b02x02p08n01i00137arch OF c04s03b02x02p08n01i00137ent IS
  type    AT0 is array (INTEGER range <>) of INTEGER;
  subtype ST0 is AT0(1 to 2);
  type    AT1 is array (INTEGER range <>) of ST0;
  subtype ST1 is AT1(1 to 2);
BEGIN
  TESTING: PROCESS

    procedure Proc1(P : inout ST1; ref : in ST1; set : in ST1) is      
    begin
      if (P = ref) then
        P := set;
      end if;
    end;
    
    variable V    : ST1 := ((1, 2), (3, 4));
    variable V1    : ST0;
    variable V2    : ST0;

  BEGIN
    V1 := (1, 2);
    V2 := (3, 4);
    Proc1(  P(1) => V2, P(2) => V1,
            ref => ((3, 4), (1, 2)), set => ((9, 8), (7, 6))); -- test here
    assert V1 = (7, 6) report "FAIL: actual V1 didn't get set right";
    assert V2 = (9, 8) report "FAIL: actual V2 didn't get set right";
    assert NOT( V1=(7,6) and V2=(9,8) )
      report "***PASSED TEST: c04s03b02x02p08n01i00137"
      severity NOTE;
    assert ( V1=(7,6) and V2=(9,8) )
      report "***FAILED TEST: c04s03b02x02p08n01i00137 - Association element in an association list test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p08n01i00137arch;
