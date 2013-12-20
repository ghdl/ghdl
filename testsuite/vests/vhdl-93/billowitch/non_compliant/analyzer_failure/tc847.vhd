
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
-- $Id: tc847.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity and2g is
end and2g;

architecture behavior of and2g is
begin
end behavior;

entity full_adder is
end full_adder;

architecture structural of full_adder is
  component and2
  end component;
begin
  C1: and2;
end structural;

ENTITY c01s03b01x00p08n01i00847ent IS
END c01s03b01x00p08n01i00847ent;

ARCHITECTURE c01s03b01x00p08n01i00847arch OF c01s03b01x00p08n01i00847ent IS
  
  component adder
  end component;
  
BEGIN
  A1 : adder;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c01s03b01x00p08n01i00847 - Architecture name in block configuration does not match block specification."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s03b01x00p08n01i00847arch;


configuration c01s03b01x00p08n01i00847cfg of c01s03b01x00p08n01i00847ent is
  for c01s03b01x00p08n01i00847arch
    for A1: adder use                      -- component configuration
                    entity work.full_adder(structural);
                  
                  for bad_block_spec       -- failure_here
                    for C1: and2 use
                                   entity work.and2g(behavior);
                    end for;
                  end for;
    end for;
  end for;
end c01s03b01x00p08n01i00847cfg;
