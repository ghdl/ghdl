
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
-- $Id: tc1838.vhd,v 1.2 2001-10-26 16:30:13 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c07s01b00x00p08n01i01838pkg is
  type    small_int    is range 0 to 7;
  type    cmd_bus    is array (small_int range <>) of small_int;
  constant    bus_width    : small_int := 7;
end c07s01b00x00p08n01i01838pkg;

use work.c07s01b00x00p08n01i01838pkg.all;
entity c07s01b00x00p08n01i01838ent_a is
  generic ( constant bus_width : small_int);
  port ( signal in_bus    : in  cmd_bus (0 to bus_width);
         signal out_bus    : out cmd_bus (0 to bus_width));
end c07s01b00x00p08n01i01838ent_a;

architecture c07s01b00x00p08n01i01838arch_a of c07s01b00x00p08n01i01838ent_a is
begin
end c07s01b00x00p08n01i01838arch_a;

use work.c07s01b00x00p08n01i01838pkg.all;
ENTITY c07s01b00x00p08n01i01838ent IS
END c07s01b00x00p08n01i01838ent;

ARCHITECTURE c07s01b00x00p08n01i01838arch OF c07s01b00x00p08n01i01838ent IS
  signal ibus, obus : cmd_bus(small_int);

  component test
    generic ( constant bus_width : natural := 7);
    port ( signal in_bus    : in  cmd_bus (0 to small_int(bus_width - 1));
           signal out_bus    : out cmd_bus (0 to small_int(bus_width - 1)));
  end component;
  for err : test use entity work.c07s01b00x00p08n01i01838ent_a(c07s01b00x00p08n01i01838arch_a);

BEGIN
  err : test generic map ( c07s01b00x00p08n01i01838ent ) -- entity name illegal here
    port map ( ibus, obus );

  TESTING : PROCESS
  BEGIN
    wait for 5 ns;
    assert FALSE
      report "***FAILED TEST: c07s01b00x00p08n01i01838 - Entity names are not permitted as primaries in a component instantiation generic map statement."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p08n01i01838arch;
