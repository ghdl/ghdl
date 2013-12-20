
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
-- $Id: tc3033.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s02b01x00p01n02i03033ent IS
END c12s02b01x00p01n02i03033ent;

ARCHITECTURE c12s02b01x00p01n02i03033arch OF c12s02b01x00p01n02i03033ent IS
  subtype subi is integer range 1 to 10;
  signal   s1,s2,s3 : subi;
BEGIN

  bl1: block
    generic (gi : subi);
    generic map (gi => 1);
    port    (s11 : OUT subi);
    port map (s11 => s1);
  begin
    assert (gi=1)
      report "Generic GI did not take on the correct low value of 1"
      severity failure;
    s11 <= gi;
  end block;
  bl2: block
    generic (gi : subi);
    generic map (gi => 5);
    port    (s22 : OUT subi);
    port map (s22 => s2);
  begin
    assert (gi=5)
      report "Generic GI did not take on the correct middle value of 5"         
      severity failure;
    s22 <= gi;
  end block;
  bl3: block
    generic (gi : subi);
    generic map (gi => 10);
    port    (s33 : OUT subi);
    port map (s33 => s3);
  begin
    assert (gi=10)
      report "Generic GI did not take on the correct high value of 10"
      severity failure;
    s33 <= gi;
  end block;

  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( s1 = 1 and s2 = 5 and s3 = 10 )
      report "***PASSED TEST: c12s02b01x00p01n02i03033"
      severity NOTE;
    assert ( s1 = 1 and s2 = 5 and s3 = 10 )
      report "***FAILED TEST: c12s02b01x00p01n02i03033 - Generic constants does not conform to their subtype indication."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s02b01x00p01n02i03033arch;
