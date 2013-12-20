
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
-- $Id: tc133.vhd,v 1.2 2001-10-26 16:29:40 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p08n01i00133ent IS
END c04s03b02x02p08n01i00133ent;

ARCHITECTURE c04s03b02x02p08n01i00133arch OF c04s03b02x02p08n01i00133ent IS
  type RT1 is record
                a : INTEGER;
                b : INTEGER;
              end record;
BEGIN
  TESTING: PROCESS

    procedure Proc1(P : inout RT1; ref : in RT1; set : in RT1) is
    begin
      if (P = ref) then
        P := set;
      end if;
    end;
    
    variable V : RT1 := (1, 2);

  BEGIN
    V := (1, 2);
    Proc1(P.a => V.b, P.b => V.a, ref => (2, 1), set => (2, 3));
                                        -- test here
    assert V = (3, 2) report "FAIL: P didn't get set right";
    assert NOT( V = (3,2) )
      report "***PASSED TEST: c04s03b02x02p08n01i00133"
      severity NOTE;
    assert ( V = (3,2) )
      report "***FAILED TEST: c04s03b02x02p08n01i00133 - Association element in an association list test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p08n01i00133arch;
