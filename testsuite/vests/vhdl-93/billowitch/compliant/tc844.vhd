
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
-- $Id: tc844.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c01s03b01x00p05n01i00844ent_a is 
end c01s03b01x00p05n01i00844ent_a ;

architecture c01s03b01x00p05n01i00844arch_a of c01s03b01x00p05n01i00844ent_a is
begin
  A1_BLK : block
    signal S : INTEGER;
  begin
    S <= 1;
  end block;
end c01s03b01x00p05n01i00844arch_a;


ENTITY c01s03b01x00p05n01i00844ent IS
END c01s03b01x00p05n01i00844ent;

architecture c01s03b01x00p05n01i00844arch of  c01s03b01x00p05n01i00844ent is
BEGIN

  AA_BLK : block
    component FOUR
    end component;
  begin
    LH : FOUR;
  end block;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c01s03b01x00p05n01i00844"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c01s03b01x00p05n01i00844arch;

configuration c01s03b01x00p05n01i00844cfg of c01s03b01x00p05n01i00844ent is
  for c01s03b01x00p05n01i00844arch
    for AA_BLK
      for LH : FOUR
        use
          entity work.c01s03b01x00p05n01i00844ent_a(c01s03b01x00p05n01i00844arch_a);
      end for;
    end for;
  end for ;
end c01s03b01x00p05n01i00844cfg;
