
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
-- $Id: tc3134.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s02b01x02p12n01i03134ent_a IS
  generic ( g1 : integer := 0 );
END c05s02b01x02p12n01i03134ent_a;

ARCHITECTURE c05s02b01x02p12n01i03134arch_a OF c05s02b01x02p12n01i03134ent_a IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert g1 /= 0    report "g1 = 0 "    severity FAILURE;
    assert g1 /= 1    report "g1 = 1 "    severity FAILURE;
    assert g1 = -1    report "g1 /= -1 "    severity FAILURE;
    assert NOT(    g1 /= 0   and
                   g1 /= 1   and
                   g1 = -1   )   
      report "***PASSED TEST: c05s02b01x02p12n01i03134"
      severity NOTE;
    assert (    g1 /= 0   and
                g1 /= 1   and
                g1 = -1   )   
      report "***FAILED TEST: c05s02b01x02p12n01i03134 - An actual associated with a formal generic in a generic map aspect be an expression test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;
END c05s02b01x02p12n01i03134arch_a;




ENTITY c05s02b01x02p12n01i03134ent IS
  generic ( test_g : integer := -1 );
END c05s02b01x02p12n01i03134ent;

ARCHITECTURE c05s02b01x02p12n01i03134arch OF c05s02b01x02p12n01i03134ent IS
  component ic_socket
    generic ( local_g1 : integer := 1 );
  end component;
BEGIN
  instance : ic_socket;
END c05s02b01x02p12n01i03134arch;


configuration c05s02b01x02p12n01i03134cfg of c05s02b01x02p12n01i03134ent is
  for c05s02b01x02p12n01i03134arch
    for instance : ic_socket use entity work.c05s02b01x02p12n01i03134ent_a (c05s02b01x02p12n01i03134arch_a)
                               generic map (test_g);
    end for;
  end for;
end c05s02b01x02p12n01i03134cfg;
