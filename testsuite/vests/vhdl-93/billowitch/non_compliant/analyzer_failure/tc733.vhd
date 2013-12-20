
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
-- $Id: tc733.vhd,v 1.2 2001-10-26 16:30:27 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c01s01b01x00p05n01i00733ent_a is
  generic (
    constant gc1 : integer;
    gc2 : natural;
    constant gc3 : positive
    );
  port ( signal cent1 : in bit;
         signal cent2 : in bit
         );
end c01s01b01x00p05n01i00733ent_a;

architecture arch of c01s01b01x00p05n01i00733ent_a is
begin
  assert false 
    report "FAIL: should not compile";
end arch;

ENTITY c01s01b01x00p05n01i00733ent IS
  generic ( constant gen_con : natural := 7 );
  port ( signal ee1 : in bit;
         signal ee2 : in bit;
         signal eo1 : out bit
         );
END c01s01b01x00p05n01i00733ent;

ARCHITECTURE c01s01b01x00p05n01i00733arch OF c01s01b01x00p05n01i00733ent IS

  signal s1 : integer;
  signal s2 : natural;
  signal s3 : positive;

  component comp1
    generic (
      constant dgc1 : integer;
      variable dgc2 : natural;
      signal dgc3 : positive
      );
    port ( signal dcent1 : in bit;
           signal dcent2 : in bit
           );
  end component;

  for u1 : comp1 use
    entity work.c01s01b01x00p05n01i00733ent_a
    generic map (dgc1, dgc2, dgc3)
    port map ( dcent1, dcent2 );

BEGIN

  u1 : comp1
    generic map (3,3,3)
    port map (ee1,ee2);

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c01s01b01x00p05n01i00733 - Variable and signal declaration can not be in local generic clause in component declaration."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x00p05n01i00733arch;
