
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
-- $Id: tc843.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c01s03b01x00p05n01i00843ent_a is 
end c01s03b01x00p05n01i00843ent_a ;

architecture c01s03b01x00p05n01i00843arch_a of c01s03b01x00p05n01i00843ent_a is
begin
  AC_BLK : block
    signal B : BIT;
  begin
    B <= '1';
  end block;
end c01s03b01x00p05n01i00843arch_a;


ENTITY c01s03b01x00p05n01i00843ent IS
END c01s03b01x00p05n01i00843ent;

ARCHITECTURE c01s03b01x00p05n01i00843arch OF c01s03b01x00p05n01i00843ent IS

BEGIN
  A_BLK : block
    component C
    end component;
  begin
    L1 : C;
    L2 : C;
    L3 : C;
    L4 : C;
    L5 : C;
    L6 : C;
    L7 : C;
    L8 : C;
    L9 : C;
  end block;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c01s03b01x00p05n01i00843"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c01s03b01x00p05n01i00843arch;

configuration c01s03b01x00p05n01i00843cfg of c01s03b01x00p05n01i00843ent is
  for c01s03b01x00p05n01i00843arch
    for A_BLK
      for L1 : C
        use entity work.c01s03b01x00p05n01i00843ent_a (c01s03b01x00p05n01i00843arch_a) ;
      end for;
      
      for L2 : C
        use entity work.c01s03b01x00p05n01i00843ent_a (c01s03b01x00p05n01i00843arch_a) ;
      end for;
      
      for L3 : C
        use entity work.c01s03b01x00p05n01i00843ent_a (c01s03b01x00p05n01i00843arch_a) ;
      end for;
      
      for L4 : C
        use entity work.c01s03b01x00p05n01i00843ent_a (c01s03b01x00p05n01i00843arch_a) ;
      end for;
      
      for L5 : C
        use entity work.c01s03b01x00p05n01i00843ent_a (c01s03b01x00p05n01i00843arch_a) ;
      end for;
      
      for L6 : C
        use entity work.c01s03b01x00p05n01i00843ent_a (c01s03b01x00p05n01i00843arch_a) ;
      end for;
      
      for L7 : C
        use entity work.c01s03b01x00p05n01i00843ent_a (c01s03b01x00p05n01i00843arch_a) ;
      end for;
      
      for L8 : C
        use entity work.c01s03b01x00p05n01i00843ent_a (c01s03b01x00p05n01i00843arch_a) ;
      end for;
      
      for L9 : C
        use entity work.c01s03b01x00p05n01i00843ent_a (c01s03b01x00p05n01i00843arch_a) ;
      end for;
    end for;
  end for ;
end c01s03b01x00p05n01i00843cfg;
