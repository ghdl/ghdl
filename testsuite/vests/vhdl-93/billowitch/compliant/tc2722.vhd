
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
-- $Id: tc2722.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s04b02x00p06n01i02722ent IS
END c13s04b02x00p06n01i02722ent;

ARCHITECTURE c13s04b02x00p06n01i02722arch OF c13s04b02x00p06n01i02722ent IS
  constant a1:integer   :=16#987_654#;
  constant b1:integer   :=16#987654#;
  constant a2:integer   :=16#A_B#;
  constant b2:integer   :=16#AB#;
  constant a3:integer   :=10#7_8#;
  constant b3:integer   :=10#78#;
  constant a4:integer   :=2#11_11#;
  constant b4:integer   :=2#1111#;
  constant a5:real   :=16#C.C_D#;
  constant b5:real   :=16#C.CD#;
  constant a6:real   :=10#8.9_7#;
  constant b6:real   :=10#8.97#;
  constant a7:real   :=2#11_11.11_11#;
  constant b7:real   :=2#1111.1111#;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert (a1=b1) report "Underline affects the value of a based literal" severity failure;
    assert (a2=b2) report "Underline affects the value of a based literal" severity failure;
    assert (a3=b3) report "Underline affects the value of a based literal" severity failure;
    assert (a4=b4) report "Underline affects the value of a based literal" severity failure;
    assert (a5=b5) report "Underline affects the value of a based literal" severity failure;
    assert (a6=b6) report "Underline affects the value of a based literal" severity failure;
    assert (a7=b7) report "Underline affects the value of a based literal" severity failure;
    assert NOT(   a1=b1   and
                  a2=b2   and
                  a3=b3   and
                  a4=b4   and
                  a5=b5   and
                  a6=b6   and
                  a7=b7   )
      report "***PASSED TEST: c13s04b02x00p06n01i02722"
      severity NOTE;
    assert (   a1=b1   and
               a2=b2   and
               a3=b3   and
               a4=b4   and
               a5=b5   and
               a6=b6   and
               a7=b7   )
      report "***FAILED TEST: c13s04b02x00p06n01i02722 - An underline character inserted between adjacent digits of a based literal does not affect the value of this abstract literal."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s04b02x00p06n01i02722arch;
