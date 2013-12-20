
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
-- $Id: tc817.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c01s02b01x00p03n01i00817ent_a IS
  port (A    : IN BIT);
END c01s02b01x00p03n01i00817ent_a;

ARCHITECTURE c01s02b01x00p03n01i00817arch_a OF c01s02b01x00p03n01i00817ent_a IS

BEGIN
  TEST : PROCESS
  BEGIN
    if A = '1' then 
      null;
    end if;
    wait;
  END PROCESS TEST;

END c01s02b01x00p03n01i00817arch_a;


package c01s02b01x00p03n01i00817pkg is
  type BIT is ('0', '1');
end c01s02b01x00p03n01i00817pkg;

ENTITY c01s02b01x00p03n01i00817ent IS
  port (A    : BIT;
        B    : out BIT;
        C, D    : Boolean) ;
END c01s02b01x00p03n01i00817ent;

ARCHITECTURE c01s02b01x00p03n01i00817arch OF c01s02b01x00p03n01i00817ent IS
  
  procedure P1 is
  begin
    return;
  end P1;
  
  function F1 return BIT is
  begin
    return '0';
  end F1;
  
  type    Q  is range 10.5 to 11.5;
  subtype    R  is REAL;
  constant    C1 : REAL := 1.39;
  signal    S  : BIT;
  component E2
    port (A : in BIT);
  end component;
  for TEST : E2 use entity work.c01s02b01x00p03n01i00817ent_a(c01s02b01x00p03n01i00817arch_a);
  use WORK.c01s02b01x00p03n01i00817pkg.all;

BEGIN
  TEST : E2 port map (S);

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c01s02b01x00p03n01i00817"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c01s02b01x00p03n01i00817arch;
