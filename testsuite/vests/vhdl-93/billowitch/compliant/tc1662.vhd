
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
-- $Id: tc1662.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s01b00x00p03n01i01662ent IS
  port (A, B: inout bit);
END c09s01b00x00p03n01i01662ent;

ARCHITECTURE c09s01b00x00p03n01i01662arch OF c09s01b00x00p03n01i01662ent IS
  signal       S1, S2, S3   : bit       := '0';
  constant    gm      : natural    := 0;
BEGIN

  BL: block                          -- no_failure_here
    generic    (n: natural:= 2);
    generic map    (gm);
    port       (A, B: inout bit);
    port map    (S1, S2);
  begin
  end block BL;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c09s01b00x00p03n01i01662"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c09s01b00x00p03n01i01662arch;
