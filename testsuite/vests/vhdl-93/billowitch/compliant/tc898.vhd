
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
-- $Id: tc898.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

entity c10s02b00x00p02n02i00898ent_a is
  generic ( g : integer := 1 );
end c10s02b00x00p02n02i00898ent_a;

architecture c10s02b00x00p02n02i00898arch_a of c10s02b00x00p02n02i00898ent_a is
begin
  assert NOT( g = 6 )
    report "***PASSED TEST: c10s02b00x00p02n02i00898"
    severity NOTE;
  assert ( g = 6 )
    report "***FAILED TEST: c10s02b00x00p02n02i00898 - Wrong generic value." 
    severity ERROR;
end c10s02b00x00p02n02i00898arch_a;


ENTITY c10s02b00x00p02n02i00898ent IS
END c10s02b00x00p02n02i00898ent;

ARCHITECTURE c10s02b00x00p02n02i00898arch OF c10s02b00x00p02n02i00898ent IS
  component ic_socket
    generic ( g : integer := 5 ); -- locally declared
  end component;
  for instance : ic_socket use entity work.c10s02b00x00p02n02i00898ent_a(c10s02b00x00p02n02i00898arch_a);
BEGIN
  instance : ic_socket generic map ( 6 );
  TESTING: PROCESS
  BEGIN
    wait;
  END PROCESS TESTING;

END c10s02b00x00p02n02i00898arch;
