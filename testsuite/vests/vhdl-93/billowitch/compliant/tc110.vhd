
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
-- $Id: tc110.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x00p29n06i00110ent IS
  port (S1 : out BIT_VECTOR(0 to 3) := "1011");
END c04s03b02x00p29n06i00110ent;

ARCHITECTURE c04s03b02x00p29n06i00110arch OF c04s03b02x00p29n06i00110ent IS
  signal S2,S3 : BIT;
BEGIN
  TESTING: PROCESS
  BEGIN
    if (S1'LOW = 0) then
      S2 <= '1' after 10 ns;
    end if;

    if (S1'HIGH = 3) then
      S3 <= '1' after 10 ns;
    end if;
    wait for 20 ns;
    
    assert NOT(S2='1' and S3='1') 
      report "***PASSED TEST: c04s03b02x00p29n06i00110" 
      severity NOTE;
    assert (S2='1' and S3='1') 
      report "***FAILED TEST: c04s03b02x00p29n06i00110 - Reading of the attributes LOW and HIGH of the interface element of mode out is allowed." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x00p29n06i00110arch;
