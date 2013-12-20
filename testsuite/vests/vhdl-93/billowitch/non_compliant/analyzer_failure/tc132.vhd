
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
-- $Id: tc132.vhd,v 1.2 2001-10-26 16:30:09 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p04n01i00132ent IS
  port (
    A1 : in         Bit;
    A2 : inout      Bit;
    A3 : linkage    Bit;
    A4 : out        Bit;
    A5 : Buffer     Bit
    ) ;
END c04s03b02x02p04n01i00132ent;

ARCHITECTURE c04s03b02x02p04n01i00132arch OF c04s03b02x02p04n01i00132ent IS
  component Local
    port (
      C1 : in         Bit;
      C2 : inout      Bit;
      C3 : linkage    Bit;
      C4 : out        Bit;
      C5 : Buffer     Bit
      );
  end component;
BEGIN
  CLSI : Local port map
    (open => A1, open => A2, open => A3, open => A4, open => A5);

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c04s03b02x02p04n01i00132 - Open is not a valid formal parameter."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p04n01i00132arch;
