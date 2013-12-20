
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
-- $Id: tc743.vhd,v 1.2 2001-10-26 16:29:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s01b01x01p04n01i00743pkg is
  type arrtype is array (1 to 5) of bit;
  constant defcon1 : bit;
  constant defcon2 : integer;
  constant defcon3 : arrtype;
  constant defcon4 : boolean;
  component comp1
    generic (
      constant dgc1 : bit := defcon1;
      constant dgc2 : integer := defcon2;
      constant dgc3 : arrtype := defcon3;
      constant dgc4 : boolean := defcon4
      );
    port ( signal dcent1 : inout bit := dgc1;
           signal dcent2 : inout integer := dgc2;
           signal dcent3 : inout arrtype := dgc3;
           signal dcent4 : inout boolean := dgc4
           );
  end component;
end c01s01b01x01p04n01i00743pkg;

package body c01s01b01x01p04n01i00743pkg is
  constant defcon1 : bit := '1';
  constant defcon2 : integer := 113;
  constant defcon3 : arrtype := ('1','0','1','0','1');
  constant defcon4 : boolean := TRUE;
end c01s01b01x01p04n01i00743pkg;

use work.c01s01b01x01p04n01i00743pkg.all;
entity c01s01b01x01p04n01i00743ent_a is
  generic (
    constant gc1 : bit;
    constant gc2 : integer;
    constant gc3 : arrtype;
    constant gc4 : boolean
    );
  port (       signal cent1 : inout bit;
               signal cent2 : inout integer;
               signal cent3 : inout arrtype;
               signal cent4 : inout boolean
               );
end c01s01b01x01p04n01i00743ent_a;

architecture c01s01b01x01p04n01i00743arch_a of c01s01b01x01p04n01i00743ent_a is
begin
  p0: process
  begin
    wait for 1 ns;
    if (gc1='1') and (gc2=113) and (gc3=('1','0','1','0','1')) and (gc4) then
      assert FALSE 
        report "***PASSED TEST: c01s01b01x01p04n01i00743"
        severity NOTE;
    else
      assert FALSE
        report "***FAILED TEST: c01s01b01x01p04n01i00743 - Generic default to deferred constants." 
        severity ERROR;
    end if;
    wait;
  end process;
end c01s01b01x01p04n01i00743arch_a;

use work.c01s01b01x01p04n01i00743pkg.all;
ENTITY c01s01b01x01p04n01i00743ent IS
  generic    (    constant gen_con : integer := 1334 );
  port    (    signal ee1 : inout boolean := TRUE;
               signal ee2 : inout bit;
               signal ee3 : inout integer;
               signal ee4 : inout arrtype
               );
END c01s01b01x01p04n01i00743ent;

ARCHITECTURE c01s01b01x01p04n01i00743arch OF c01s01b01x01p04n01i00743ent IS
  for u1 : comp1 use
    entity work.c01s01b01x01p04n01i00743ent_a(c01s01b01x01p04n01i00743arch_a)
    generic map ( dgc1, dgc2, dgc3, dgc4 )
    port map ( dcent1, dcent2, dcent3, dcent4 );
BEGIN

  u1 : comp1; 

END c01s01b01x01p04n01i00743arch;
