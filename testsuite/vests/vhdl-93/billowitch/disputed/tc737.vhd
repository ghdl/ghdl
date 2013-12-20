
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
-- $Id: tc737.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c01s01b01x01p04n01i00737ent_a is
  generic (
    constant gc1 : in integer;
    constant gc2 : in real;
    constant gc3 : in boolean
    );
  port ( signal cent1 : in bit;
         signal cent2 : in bit
         );
end c01s01b01x01p04n01i00737ent_a;

architecture c01s01b01x01p04n01i00737arch_a of c01s01b01x01p04n01i00737ent_a is
begin
  p0: process
  begin
    wait for 1 ns;
    if (gc1 = 5) AND (gc2 = 0.1234) AND (gc3) then
      assert FALSE 
        report "***PASSED TEST: c01s01b01x01p04n01i00737"
        severity NOTE;
    else
      assert FALSE
        report "***FAILED TEST: c01s01b01x01p04n01i00737 - Simple generic association in component instantiation (type conversion done on actual in generic map failed)."
        severity ERROR;
    end if;
    wait;
  end process;
end c01s01b01x01p04n01i00737arch_a;


ENTITY c01s01b01x01p04n01i00737ent IS
  generic ( constant gen_con : integer := 7 );
  port ( signal ee1 : in bit;
         signal ee2 : in bit;
         signal eo1 : out bit
         );
END c01s01b01x01p04n01i00737ent;

ARCHITECTURE c01s01b01x01p04n01i00737arch OF c01s01b01x01p04n01i00737ent IS
  constant    c1 : integer    := 33;
  constant    c2 : real    := 1.23557;
  constant    c3 : boolean    := FALSE;
  signal       s1 : integer;
  signal       s2 : integer;
  signal       s3 : integer;

  component comp1
    generic (
      constant dgc1 : integer;
      constant dgc2 : real;
      constant dgc3 : boolean
      );
    port ( signal dcent1 : in bit;
           signal dcent2 : in bit
           );
  end component;
  
  for u1 : comp1 use
    entity work.c01s01b01x01p04n01i00737ent_a(c01s01b01x01p04n01i00737_arch_a)
    generic map (dgc1, dgc2, dgc3)
    port map ( dcent1, dcent2 );

  function BoolToInt(bin : boolean) return integer is
  begin
    if bin then
      return 5;
    else
      return 99;
    end if;
  end;
  
  function IntegerToReal(iin : integer) return real is
  begin
    return 0.1234;
  end;
  
  function BitToBool(bin : bit) return boolean is
  begin
    if (bin = '1') then
      return TRUE;
    else
      return FALSE;
    end if;
  end;

BEGIN
  
  u1 : comp1
    generic map (BoolToInt(TRUE), IntegerToReal(1234), BitToBool('1'))
    port map (ee1,ee2);

END c01s01b01x01p04n01i00737arch;
