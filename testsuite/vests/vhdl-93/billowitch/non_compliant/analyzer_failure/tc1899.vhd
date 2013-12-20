
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
-- $Id: tc1899.vhd,v 1.2 2001-10-26 16:30:14 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c07s01b00x00p08n01i01899pkg is
  type    small_int    is range 0 to 7;
  type    cmd_bus    is array (small_int range <>) of small_int;
  constant    bus_width    : small_int := 7;
end c07s01b00x00p08n01i01899pkg;


use work.c07s01b00x00p08n01i01899pkg.all;
entity c07s01b00x00p08n01i01899ent_a is
  generic    ( constant bus_width : small_int);
  port    ( signal in_bus : in cmd_bus (0 to bus_width);
            signal out_bus : out cmd_bus (0 to bus_width));
end c07s01b00x00p08n01i01899ent_a;

architecture c07s01b00x00p08n01i01899arch_a of c07s01b00x00p08n01i01899ent_a is
begin
  assert true ;
end c07s01b00x00p08n01i01899arch_a;


use work.c07s01b00x00p08n01i01899pkg.all;
ENTITY c07s01b00x00p08n01i01899ent IS
END c07s01b00x00p08n01i01899ent;

ARCHITECTURE c07s01b00x00p08n01i01899arch OF c07s01b00x00p08n01i01899ent IS

  constant    bus_width    : natural    := 8;
  signal    s_int       : small_int    := 0;
  signal    ibus, obus, obus2 : cmd_bus(small_int);

  component test
    generic ( constant bus_width : small_int := 5 );
    port ( signal in_bus    : in  cmd_bus (0 to small_int(bus_width));
           signal out_bus    : out cmd_bus (0 to small_int(bus_width)));
  end component;

BEGIN
  b: block ( s_int = 0 )
    signal bool : boolean := false;
    
    function value return small_int is
      variable tmp : small_int := 0;
    begin
      case tmp is
        when 0 =>
          tmp := 0;
        when others =>
          tmp := 1;
      end case;
      
      return tmp;
    end value;
    
    for c : test use entity work.c07s01b00x00p08n01i01899ent_a(c07s01b00x00p08n01i01899arch_a);
  begin
    obus    <= (0 => 1, others => value) after 5 ns;
    s: bool <= s_int = ibus'right(1) after 5 ns;
    
    c : test
      generic map ( b ) --block labels illegal here
      port map ( ibus, obus2 );
    
    p: process ( s_int )
    begin
      l: for i in small_int loop
        assert false
          report "body name accepted as primary in a component instantiation generic map expression."
          severity note ;
        exit l;
      end loop l;
    end process p;
  end block b;

  TESTING : PROCESS
  BEGIN
    wait for 5 ns;
    assert FALSE
      report "***FAILED TEST: c07s01b00x00p08n01i01899d - Block labels are not permitted as primaries in a component instantiation generic map expression."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p08n01i01899arch;
