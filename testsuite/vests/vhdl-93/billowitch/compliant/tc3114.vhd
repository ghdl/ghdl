
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
-- $Id: tc3114.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s02b00x00p07n01i03114ent_a IS
  generic ( g1 : boolean );
  port    ( p1 : in  Bit;
            p2 : out Bit );
END c05s02b00x00p07n01i03114ent_a;

ARCHITECTURE c05s02b00x00p07n01i03114arch_a OF c05s02b00x00p07n01i03114ent_a IS

BEGIN
  p2 <= p1 after 10 ns;
END c05s02b00x00p07n01i03114arch_a;

configuration c05s02b00x00p07n01i03114cfg_a of c05s02b00x00p07n01i03114ent_a is
  for c05s02b00x00p07n01i03114arch_a
  end for;
end c05s02b00x00p07n01i03114cfg_a;



ENTITY c05s02b00x00p07n01i03114ent IS
END c05s02b00x00p07n01i03114ent;

ARCHITECTURE c05s02b00x00p07n01i03114arch OF c05s02b00x00p07n01i03114ent IS
  component virtual
    generic ( g1 : boolean );
    port    ( p1 : in  Bit;
              p2 : out Bit );
  end component;
  
  for u1     : virtual  use entity work.c05s02b00x00p07n01i03114ent_a(c05s02b00x00p07n01i03114arch_a);
  for others : virtual  use entity work.c05s02b00x00p07n01i03114ent_a(c05s02b00x00p07n01i03114arch_a);
  
  signal s1,s2,s3,s4 : Bit;
BEGIN

  u1 : virtual
    generic map ( true )
    port map (s1, s2);
  u2 : virtual
    generic map ( true )
    port map (s2, s3);
  u3 : virtual
    generic map ( true )
    port map (s3, s4);
  
  TESTING: PROCESS
  BEGIN
    wait for 30 ns;
    assert NOT(    s2 = s1   and
                   s3 = s2   and
                   s4 = s3   )
      report "***PASSED TEST: c05s02b00x00p07n01i03114"
      severity NOTE;
    assert (    s2 = s1   and
                s3 = s2   and
                s4 = s3   )
      report "***FAILED TEST: c05s02b00x00p07n01i03114 - The use of the others clause did not properly configure an instance which has not been previously configured in a configuration specification in an architecture declarative region."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s02b00x00p07n01i03114arch;


configuration c05s02b00x00p07n01i03114cfg of c05s02b00x00p07n01i03114ent is
  for c05s02b00x00p07n01i03114arch
  end for;
end c05s02b00x00p07n01i03114cfg;
