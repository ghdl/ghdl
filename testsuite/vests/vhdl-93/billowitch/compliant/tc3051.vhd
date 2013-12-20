
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
-- $Id: tc3051.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s02b04x00p03n01i03051ent IS
END c12s02b04x00p03n01i03051ent;

ARCHITECTURE c12s02b04x00p03n01i03051arch OF c12s02b04x00p03n01i03051ent IS
  signal si:integer   := 14;
  signal sr:real   := 1.4;
  signal sb:bit   := '0';
BEGIN
  -- test for middle port associated
  bl3: block
    port (i:integer:=4;r:real:=6.4;b:bit:='1');
    port map (r=>sr);
  begin
    assert (i=4)
      report "Default expression for unassociated integer port I incorrect"
      severity failure;
    assert (b='1')
      report "Default expression for unassociated bit port B incorrect"         
      severity failure;
    TESTING: PROCESS
    BEGIN
      assert NOT( i=4 and r=1.4 and b='1' )
        report "***PASSED TEST: c12s02b04x00p03n01i03051"
        severity NOTE;
      assert ( i=4 and r=1.4 and b='1' )
        report "***FAILED TEST: c12s02b04x00p03n01i03051 - Unassociated and associated ports are not correctly evaluated for the ports of a block."
        severity ERROR;
      wait;
    END PROCESS TESTING;
  end block;

END c12s02b04x00p03n01i03051arch;
