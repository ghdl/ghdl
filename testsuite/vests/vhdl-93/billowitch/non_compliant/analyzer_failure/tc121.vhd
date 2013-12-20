
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
-- $Id: tc121.vhd,v 1.2 2001-10-26 16:30:07 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x00p29n15i00121ent_a IS
  port (   cpt1 : in      BIT;
           cpt2 : inout   BIT;
           cpt3 : out     BIT;
           cpt4 : buffer  BIT;
           cpt5 : linkage BIT);
END c04s03b02x00p29n15i00121ent_a;

ARCHITECTURE c04s03b02x00p29n15i00121arch_a OF c04s03b02x00p29n15i00121ent_a IS
BEGIN
END c04s03b02x00p29n15i00121arch_a;




ENTITY c04s03b02x00p29n15i00121ent IS
  port (   lpt1 : linkage BIT;
           lpt2 : linkage BIT;
           lpt3 : linkage BIT;
           lpt4 : linkage BIT;
           lpt5 : linkage BIT;
           lpt6 : linkage BIT) ;
END c04s03b02x00p29n15i00121ent;

ARCHITECTURE c04s03b02x00p29n15i00121arch OF c04s03b02x00p29n15i00121ent IS
  component com1 
    port (   cpt1 : in      BIT;
             cpt2 : inout   BIT;
             cpt3 : out     BIT;
             cpt4 : buffer  BIT;
             cpt5 : linkage BIT);
  end component;
  for CIS : com1 use entity work.ch040302_p03401_03_01_ent_a(ch040302_p03401_03_01_arch_a);
BEGIN
  CIS : com1 port map (cpt1 => lpt2, -- in formal  -- Failure_here
                       -- ERROR: Interface elements of mode linkage may not be read except
                       -- by association with formal linkage ports of subcomponents.
                       
                       cpt2 => lpt3, -- inout formal  -- Failure_here
                       -- ERROR: Interface elements of mode linkage may not be read except
                       -- by association with formal linkage ports of subcomponents.
                       
                       cpt3 => lpt4, -- out formal  -- Failure_here
                       -- ERROR: Interface elements of mode linkage may not be read except
                       -- by association with formal linkage ports of subcomponents.
                       
                       cpt4 => lpt5, -- buffer formal  -- Failure_here
                       -- ERROR: Interface elements of mode linkage may not be read except
                       -- by association with formal linkage ports of subcomponents.
                       
                       cpt5 => lpt6);

  TESTING: PROCESS
  BEGIN
    assert FALSE
      report "***FAILED TEST: c04s03b02x00p29n15i00121 - Reading and updating are not permitted on this mode."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x00p29n15i00121arch;
