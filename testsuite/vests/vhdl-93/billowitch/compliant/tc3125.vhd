
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
-- $Id: tc3125.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s02b01x02p08n01i03125ent_a IS
  generic ( message : string );
END c05s02b01x02p08n01i03125ent_a;

ARCHITECTURE c05s02b01x02p08n01i03125arch_a OF c05s02b01x02p08n01i03125ent_a IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( message = "Hello there..." )
      report "***PASSED TEST: c05s02b01x02p08n01i03125"
      severity NOTE;
    assert ( message = "Hello there..." )
      report "***FAILED TEST: c05s02b01x02p08n01i03125 - Generic maps work does not work properly with unconstrained generic elements."
      severity ERROR;
    wait;
  END PROCESS TESTING;
END c05s02b01x02p08n01i03125arch_a;



ENTITY c05s02b01x02p08n01i03125ent IS
END c05s02b01x02p08n01i03125ent;

ARCHITECTURE c05s02b01x02p08n01i03125arch OF c05s02b01x02p08n01i03125ent IS
  component ic_socket
    generic ( message : string );
  end component;
BEGIN
  u1 : ic_socket generic map ("Hello there...");

END c05s02b01x02p08n01i03125arch;


configuration c05s02b01x02p08n01i03125cfg of c05s02b01x02p08n01i03125ent is
  for c05s02b01x02p08n01i03125arch
    for u1 : ic_socket use entity work.c05s02b01x02p08n01i03125ent_a(c05s02b01x02p08n01i03125arch_a);
    end for;
  end for;
end c05s02b01x02p08n01i03125cfg;
