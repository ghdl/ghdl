
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
-- $Id: tc3152.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s02b02x00p08n01i03152ent_a IS
END c05s02b02x00p08n01i03152ent_a;

ARCHITECTURE c05s02b02x00p08n01i03152arch_a OF c05s02b02x00p08n01i03152ent_a IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c05s02b02x00p08n01i03152 - The architecture body is not the most recently analyzed architecture body associated with the entity declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s02b02x00p08n01i03152arch_a;


ARCHITECTURE c05s02b02x00p08n01i03152arch_b OF c05s02b02x00p08n01i03152ent_a IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c05s02b02x00p08n01i03152 - The architecture body is not the most recently analyzed architecture body associated with the entity declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s02b02x00p08n01i03152arch_b;



ENTITY c05s02b02x00p08n01i03152ent_c IS
END c05s02b02x00p08n01i03152ent_c;

ARCHITECTURE c05s02b02x00p08n01i03152arch_c OF c05s02b02x00p08n01i03152ent_c IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***PASSED TEST: c05s02b02x00p08n01i03152"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c05s02b02x00p08n01i03152arch_c;



ENTITY c05s02b02x00p08n01i03152ent IS
END c05s02b02x00p08n01i03152ent;

ARCHITECTURE c05s02b02x00p08n01i03152arch OF c05s02b02x00p08n01i03152ent IS
  component c05s02b02x00p08n01i03152ent_a
  end component;
BEGIN
  comp1 : c05s02b02x00p08n01i03152ent_a;

END c05s02b02x00p08n01i03152arch;



configuration c05s02b02x00p08n01i03152cfg of c05s02b02x00p08n01i03152ent is
  for c05s02b02x00p08n01i03152arch
    for comp1 : c05s02b02x00p08n01i03152ent_a use entity work.c05s02b02x00p08n01i03152ent_c;
    end for;
  end for;
end c05s02b02x00p08n01i03152cfg;
