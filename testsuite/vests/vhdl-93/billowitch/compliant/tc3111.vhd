
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
-- $Id: tc3111.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s02b00x00p06n01i03111ent_a IS
  generic ( g1 : boolean );
  port    ( p1 : in  Bit;
            p2 : out Bit );
END c05s02b00x00p06n01i03111ent_a;

ARCHITECTURE c05s02b00x00p06n01i03111arch_a OF c05s02b00x00p06n01i03111ent_a IS

BEGIN
  p2 <= p1 after 10 ns;
END c05s02b00x00p06n01i03111arch_a;



ENTITY c05s02b00x00p06n01i03111ent IS
END c05s02b00x00p06n01i03111ent;

ARCHITECTURE c05s02b00x00p06n01i03111arch OF c05s02b00x00p06n01i03111ent IS
  signal       s1   : Bit := '0';
  signal       s2   : Bit := '1';
  component virtual
    generic ( g1 : boolean );
    port    ( p1 : in  Bit;
              p2 : out Bit );
  end component;
  for u1 : virtual use entity work.c05s02b00x00p06n01i03111ent_a (c05s02b00x00p06n01i03111arch_a);
BEGIN

  u1 : virtual
    generic map ( true ) port map (s1, s2);

  TESTING: PROCESS
  BEGIN
    wait for 50 ns;
    assert NOT( s2 = s1 )
      report "***PASSED TEST: c05s02b00x00p06n01i03111"
      severity NOTE;
    assert ( s2 = s1 )
      report "***FAILED TEST: c05s02b00x00p06n01i03111 - Component instantiation test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s02b00x00p06n01i03111arch;
