
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
-- $Id: tc875.vhd,v 1.2 2001-10-26 16:30:28 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c01s03b02x00p02n01i00875ent_a  is
  port (   ia, ib : bit;
           oc, od : out bit) ;
end c01s03b02x00p02n01i00875ent_a;

architecture c01s03b02x00p02n01i00875arch_a of c01s03b02x00p02n01i00875ent_a is
begin
  A1_BLK : block
    signal S : INTEGER;
  begin
    S <= 1;
  end block;
end c01s03b02x00p02n01i00875arch_a;

ENTITY c01s03b02x00p02n01i00875ent IS
  port (   P3 : out bit;
           P4 : out bit) ;
END c01s03b02x00p02n01i00875ent;

ARCHITECTURE c01s03b02x00p02n01i00875arch OF c01s03b02x00p02n01i00875ent IS
BEGIN
  BB : block
    signal S1 : bit;
    signal S2 : bit;
    component LOCAL port(   CI, I2    : in BIT;
                            CO, RES :out BIT);
    end component ;
    
    for         --- Failure_here
      use entity work.c01s03b02x00p02n01i00875ent_a (c01s03b02x00p02n01i00875arch_a)
      port map (ia => CI, ib => I2, oc => CO, od => RES);
  begin
    L : LOCAL port map (CI =>S1 , I2 =>S2 , CO=>P3 , RES =>P4 );
    assert FALSE 
      report "***FAILED TEST: c01s03b02x00p02n01i00875 - Missing component specification."
      severity ERROR;
  end block BB;

END c01s03b02x00p02n01i00875arch;
