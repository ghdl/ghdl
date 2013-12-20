
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
-- $Id: tc738.vhd,v 1.2 2001-10-26 16:30:27 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c01s01b01x01p04n03i00738ent_a is
  generic (
    constant gc1 : integer;
    constant gc2 : natural;
    constant gc3 : positive
    );
  port ( signal cent1 : in bit;
         signal cent2 : in bit
         );
end c01s01b01x01p04n03i00738ent_a;

architecture arch of c01s01b01x01p04n03i00738ent_a is
begin
  assert false 
    report "FAIL: should not compile";
end arch;

ENTITY c01s01b01x01p04n03i00738ent IS
  generic ( constant gen_con : natural := 7 );
  port ( signal ee1 : in bit;
         signal ee2 : in bit;
         signal eo1 : out bit
         );
END c01s01b01x01p04n03i00738ent;

ARCHITECTURE c01s01b01x01p04n03i00738arch OF c01s01b01x01p04n03i00738ent IS

  signal s1 : integer;
  signal s2 : natural;
  signal s3 : positive;

  component comp1
    generic (
      constant dgc1 : integer;
      constant dgc2 : natural;
      constant dgc3 : positive
      );
    port ( signal dcent1 : in bit;
           signal dcent2 : in bit
           );
  end component;

  for u1 : comp1 use entity work.c01s01b01x01p04n03i00738ent_a(arch)
    generic map (dgc1, dgc2, dgc3)
    port map ( dcent1, dcent2 );

BEGIN

  u1 : comp1
    generic map (s1,s2,s3)
    port map (ee1,ee2);

  TESTING: PROCESS
  BEGIN

    assert FALSE 
      report "***FAILED TEST: c01s01b01x01p04n03i00738 - Formal generic should have actual or default expression."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p04n03i00738arch;
