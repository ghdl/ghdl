
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
-- $Id: tc742.vhd,v 1.2 2001-10-26 16:29:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s01b01x01p04n01i00742pkg is
  type arrtype is array (1 to 5) of integer;
  type rectype is record
-- 'a',33,0.1234,TRUE
                    ch : character;
                    int : integer;
                    re : real;
                    bo : boolean;
                  end record;
end c01s01b01x01p04n01i00742pkg;

use work.c01s01b01x01p04n01i00742pkg.all;
entity c01s01b01x01p04n01i00742ent_a is
  generic (
    constant gc1 : arrtype;
    constant gc2 : rectype;
    constant gc3 : boolean
    );
  port ( signal cent1 : in bit;
         signal cent2 : in bit
         );
end c01s01b01x01p04n01i00742ent_a;

architecture c01s01b01x01p04n01i00742arch_a of c01s01b01x01p04n01i00742ent_a is
begin
  p0: process
  begin
    wait for 1 ns;
    if (gc1=(1,2,3,4,5)) AND (gc2.ch='a') AND (gc2.int=33) AND (gc2.re=0.1234) AND (gc2.bo)  AND (gc3) then
      assert FALSE 
        report "***PASSED TEST: c01s01b01x01p04n01i00742"
        severity NOTE;
    else
      assert FALSE
        report "***FAILED TEST: c01s01b01x01p04n01i00742 - Generic association with type conversion in component instantiation failed." 
        severity ERROR;
    end if;
    wait;
  end process;
end c01s01b01x01p04n01i00742arch_a;

use work.c01s01b01x01p04n01i00742pkg.all;
ENTITY c01s01b01x01p04n01i00742ent IS
  generic ( constant gen_con : integer := 7 );
  port ( signal ee1 : in bit;
         signal ee2 : in bit;
         signal eo1 : out bit
         );
END c01s01b01x01p04n01i00742ent;

ARCHITECTURE c01s01b01x01p04n01i00742arch OF c01s01b01x01p04n01i00742ent IS
  signal       s1 : integer;
  signal       s2 : integer;
  signal       s3 : integer;

  component comp1
    generic (
      constant dgc1 : arrtype;
      constant dgc2 : rectype;
      constant dgc3 : boolean
      );
    port ( signal dcent1 : in bit;
           signal dcent2 : in bit
           );
  end component;
  
  for u1 : comp1 use
    entity work.c01s01b01x01p04n01i00742ent_a(c01s01b01x01p04n01i00742arch_a)
    generic map (dgc1, dgc2, dgc3)
    port map ( dcent1, dcent2 );

  function BoolToArr(bin : boolean) return arrtype is
  begin
    if bin then
      return (1,2,3,4,5);
    else
      return (9,8,7,6,5);
    end if;
  end;
  
  function IntegerToRec(iin : integer) return rectype is
  begin
    return ('a',33,0.1234,TRUE);
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
    generic map (BoolToArr(TRUE), IntegerToRec(1234), BitToBool('1'))
    port map (ee1,ee2);

END c01s01b01x01p04n01i00742arch;
