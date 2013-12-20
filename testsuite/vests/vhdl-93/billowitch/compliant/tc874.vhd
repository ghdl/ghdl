
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
-- $Id: tc874.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c01s03b01x00p17n02i00874ent_a is
end c01s03b01x00p17n02i00874ent_a;

architecture c01s03b01x00p17n02i00874arch_a of c01s03b01x00p17n02i00874ent_a is
begin
end c01s03b01x00p17n02i00874arch_a;

entity c01s03b01x00p17n02i00874ent_b is
end c01s03b01x00p17n02i00874ent_b;

architecture c01s03b01x00p17n02i00874arch_b of c01s03b01x00p17n02i00874ent_b is
  component c01s03b01x00p17n02i00874ent_a
  end component;
begin
  C1: c01s03b01x00p17n02i00874ent_a;
end c01s03b01x00p17n02i00874arch_b;

ENTITY c01s03b01x00p17n02i00874ent IS
END c01s03b01x00p17n02i00874ent;

ARCHITECTURE c01s03b01x00p17n02i00874arch OF c01s03b01x00p17n02i00874ent IS

  component adder
  end component;
  
BEGIN
  A1 : adder;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c01s03b01x00p17n02i00874"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c01s03b01x00p17n02i00874arch;

configuration c01s03b01x00p17n02i00874cfg of c01s03b01x00p17n02i00874ent is
  for c01s03b01x00p17n02i00874arch
    for A1: adder use             -- component configuration
                    entity work.c01s03b01x00p17n02i00874ent_b(c01s03b01x00p17n02i00874arch_b);
                  
                  for c01s03b01x00p17n02i00874arch_b          -- no_failure_here    block configuration
                    -- implicit component configuration
                  end for;           -- no_failure_here
    end for;
  end for;
end c01s03b01x00p17n02i00874cfg;
