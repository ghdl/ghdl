
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
-- $Id: tc3034.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s02b01x00p01n02i03034ent IS
END c12s02b01x00p01n02i03034ent;

ARCHITECTURE c12s02b01x00p01n02i03034arch OF c12s02b01x00p01n02i03034ent IS
  subtype subi is integer range 1   to 10;
  subtype subr is real    range 1.0 to 10.0;
  subtype subb is bit     range '1' to '1';

  type c_r is
    record
      i : subi;
      r : subr;
      b : subb;
    end record;

  signal s1, s2, s3 : c_r;
BEGIN
  -- test record generics
  bl4: block
    generic(gr : c_r);
    generic map (gr => (1,1.0,'1'));
    port   (s11 : OUT c_r);
    port map (s11 => s1);
  begin
    assert ((gr.i=1) and (gr.r=1.0) and (gr.b='1'))
      report "Generic record GR did not take on the correct low value"
      severity failure;
    s11 <= gr;
  end block;
  bl5: block
    generic(gr : c_r);
    generic map (gr => (5,5.0,'1'));
    port   (s22 : OUT c_r);
    port map (s22 => s2);
  begin
    assert ((gr.i=5) and (gr.r=5.0) and (gr.b='1'))
      report "Generic record GR did not take on the correct middle value"
      severity failure;
    s22 <= gr;
  end block;
  bl6: block
    generic(gr : c_r);
    generic map (gr => (10,10.0,'1'));
    port   (s33 : OUT c_r);
    port map (s33 => s3);
  begin
    assert ((gr.i=10) and (gr.r=10.0) and (gr.b='1'))
      report "Generic record GR did not take on the correct high value"         
      severity failure;
    s33 <= gr;
  end block;

  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( s1 = (1,1.0,'1') and s2 = (5,5.0,'1') and s3 = (10,10.0,'1') )
      report "***PASSED TEST: c12s02b01x00p01n02i03034"
      severity NOTE;
    assert ( s1 = (1,1.0,'1') and s2 = (5,5.0,'1') and s3 = (10,10.0,'1') )
      report "***FAILED TEST: c12s02b01x00p01n02i03034 - Generic constants does not conform to their subtype indication."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s02b01x00p01n02i03034arch;
