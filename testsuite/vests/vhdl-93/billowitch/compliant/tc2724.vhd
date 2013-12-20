
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
-- $Id: tc2724.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c13s04b02x00p06n04i02724ent IS
END c13s04b02x00p06n04i02724ent;

ARCHITECTURE c13s04b02x00p06n04i02724arch OF c13s04b02x00p06n04i02724ent IS
  constant a1   :integer:=16#A#;
  constant b1   :integer:=16#a#;
  constant a2   :integer:=16#B#;
  constant b2   :integer:=16#b#;
  constant a3   :integer:=16#C#;
  constant b3   :integer:=16#c#;
  constant a4   :integer:=16#D#;
  constant b4   :integer:=16#d#;
  constant a5   :integer:=16#E#;
  constant b5   :integer:=16#e#;
  constant a6   :integer:=16#F#;
  constant b6   :integer:=16#f#;

  constant a7   :real:=16#A.A#;
  constant b7   :real:=16#a.a#;
  constant a8   :real:=16#B.B#;
  constant b8   :real:=16#b.b#;
  constant a9   :real:=16#C.C#;
  constant b9   :real:=16#c.c#;
  constant a10   :real:=16#D.D#;
  constant b10   :real:=16#d.d#;
  constant a11   :real:=16#E.E#;
  constant b11   :real:=16#e.e#;
  constant a12   :real:=16#F.F#;
  constant b12   :real:=16#f.f#;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert (a1=b1) report "A & a not the same for an extended digit in an integer based literal" severity failure;
    assert (a2=b2) report "B & b not the same for an extended digit in an integer based literal" severity failure;
    assert (a3=b3) report "C & c not the same for an extended digit in an integer based literal" severity failure;
    assert (a4=b4) report "D & d not the same for an extended digit in an integer based literal" severity failure;
    assert (a5=b5) report "E & e not the same for an extended digit in an integer based literal" severity failure;
    assert (a6=b6) report "F & f not the same for an extended digit in an integer based literal" severity failure;
    
    assert (a7=b7) report "A & a not the same for an extended digit in a real based literal" severity failure;
    assert (a8=b8) report "B & b not the same for an extended digit in a real based literal" severity failure;
    assert (a9=b9) report "C & c not the same for an extended digit in a real based literal" severity failure;
    assert (a10=b10) report "D & d not the same for an extended digit in a real based literal" severity failure;
    assert (a11=b11) report "E & e not the same for an extended digit in a real based literal" severity failure;
    assert (a12=b12) report "F & f not the same for an extended digit in a real based literal" severity failure;
    assert NOT(    a1   = b1    and
                   a2   = b2   and
                   a3   = b3   and
                   a4   = b4   and
                   a5   = b5   and
                   a6   = b6   and
                   a7   = b7   and
                   a8   = b8   and
                   a9   = b9   and
                   a10   = b10   and
                   a11   = b11   and
                   a12   = b12   )
      report "***PASSED TEST: c13s04b02x00p06n04i02724"
      severity NOTE;
    assert (    a1   = b1    and
                a2   = b2   and
                a3   = b3   and
                a4   = b4   and
                a5   = b5   and
                a6   = b6   and
                a7   = b7   and
                a8   = b8   and
                a9   = b9   and
                a10   = b10   and
                a11   = b11   and
                a12   = b12   )
      report "***FAILED TEST: c13s04b02x00p06n04i02724 - Upper and lower case should be allowed for a letter in integer and real based literal with the came meaning."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c13s04b02x00p06n04i02724arch;
