
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
-- $Id: tc1900.vhd,v 1.2 2001-10-26 16:30:14 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s01b00x00p08n01i01900ent_a IS
  generic ( constant bus_width : natural);
  port ( signal in_bus : in integer;
         signal out_bus : out integer);
END c07s01b00x00p08n01i01900ent_a;

ARCHITECTURE c07s01b00x00p08n01i01900arch_a OF c07s01b00x00p08n01i01900ent_a IS
BEGIN
  assert true;
END c07s01b00x00p08n01i01900arch_a;


ENTITY c07s01b00x00p08n01i01900ent IS
END c07s01b00x00p08n01i01900ent;

ARCHITECTURE c07s01b00x00p08n01i01900arch OF c07s01b00x00p08n01i01900ent IS

  constant    bus_width       : natural:= 8;
  signal    s_int          : integer;
  signal    ibus, obus, obus2    : integer;

  component test
    generic ( constant bus_width : natural := 5 );
    port ( signal in_bus    : in  integer; 
           signal out_bus    : out integer ); 
  end component;

BEGIN
  b: block ( s_int = 0 )
    for c2 : test use entity work.ch0701_p00801_91_ent_a(c07s01b00x00p08n01i01900arch_a);
  begin
    p: process ( s_int )
    begin
      l: for i in 0 to 7 loop
        assert false
          report "process labels accepted as primary in a component instantiation generic map expression."
          severity note ;
        exit l;
      end loop l;
    end process p;
    c2 : test    generic map (p) -- process label illegal here
      port map (ibus, obus2);
  end block b;

  TESTING : PROCESS
  BEGIN
    wait for 5 ns;
    assert FALSE
      report "***FAILED TEST: c07s01b00x00p08n01i01900 - Process labels are not permitted as primaries in a component instantiation generic map expression."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s01b00x00p08n01i01900arch;
