
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
-- $Id: tc1653.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c09s00b00x00p02n01i01653ent_a is
  port (signal ss : in integer);
end c09s00b00x00p02n01i01653ent_a;

architecture c09s00b00x00p02n01i01653arch_a of c09s00b00x00p02n01i01653ent_a is
begin
  process
  begin
    wait;
  end process;
end c09s00b00x00p02n01i01653arch_a;

ENTITY c09s00b00x00p02n01i01653ent IS
  port ( Pt    : in BOOLEAN;
         PTO    : out BIT) ;
END c09s00b00x00p02n01i01653ent;

ARCHITECTURE c09s00b00x00p02n01i01653arch OF c09s00b00x00p02n01i01653ent IS

  component FO 
    port (signal ss : in INTEGER);
  end component ;
  for Ls : FO use entity work.c09s00b00x00p02n01i01653ent_a(c09s00b00x00p02n01i01653arch_a);

  signal S1, S2    : Integer;
  signal S       : INTEGER;

BEGIN
  -- concurrent signal statement
  S <= transport 5;
  
  -- concurrent assertion statement
  assert ( not PT)
    report " dead wire "
    severity WARNING;
  
  -- generate
  L_G_1:  for I in 1 to 1 generate
    L_X_2:    block
      signal S3 : Bit;
    begin
      S2 <= transport 1;
    end block;
  end generate;
  
  -- component instatiation
  Ls : FO port map (S1);

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c09s00b00x00p02n01i01653" 
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c09s00b00x00p02n01i01653arch;
