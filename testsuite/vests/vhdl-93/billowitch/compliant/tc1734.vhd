
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
-- $Id: tc1734.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s04b00x00p02n01i01734ent IS
begin
  l1: assert false
    report "Labeled concurrent assert OK in entity."
    severity note ;
  assert false
    report "Unlabeled concurrent assert OK in entity."
    severity note ;
END c09s04b00x00p02n01i01734ent;

ARCHITECTURE c09s04b00x00p02n01i01734arch OF c09s04b00x00p02n01i01734ent IS

BEGIN
  l2: assert false
    report "Labeled concurrent assert OK in architecture."
    severity note ;
  assert false
    report "Unlabeled concurrent assert OK in architecture."
    severity note ;

  B : block
  BEGIN
    l1: assert false
      report "Labeled concurrent assert OK in block."
      severity note ;
    assert false
      report "Unlabeled concurrent assert OK in block."
      severity note ;

    assert FALSE 
      report "***PASSED TEST: c09s04b00x00p02n01i01734 - This test is passed if and only if we get other six assertion sentence."
      severity NOTE;
  end block B;

END c09s04b00x00p02n01i01734arch;




