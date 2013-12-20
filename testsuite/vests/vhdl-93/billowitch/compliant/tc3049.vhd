
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
-- $Id: tc3049.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s02b04x00p03n01i03049ent_a IS
  port(con : in BIT := '1'; clk : out BIT);
END c12s02b04x00p03n01i03049ent_a;

ARCHITECTURE c12s02b04x00p03n01i03049arch_a OF c12s02b04x00p03n01i03049ent_a IS
BEGIN
  process(con)
  begin
    clk <= con;
  end process;
END c12s02b04x00p03n01i03049arch_a;


ENTITY c12s02b04x00p03n01i03049ent IS
END c12s02b04x00p03n01i03049ent;

ARCHITECTURE c12s02b04x00p03n01i03049arch OF c12s02b04x00p03n01i03049ent IS
  signal C : bit := '0';
  component c12s02b04x00p03n01i03049ent_aa
    port (   con : IN    bit := '1';
             clk : OUT   bit   );
  end component;   
  for all : c12s02b04x00p03n01i03049ent_aa use entity work.c12s02b04x00p03n01i03049ent_a(c12s02b04x00p03n01i03049arch_a);
BEGIN

  T1 : c12s02b04x00p03n01i03049ent_aa port map (open, C);

  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( C = '1' )
      report "***PASSED TEST: c12s02b04x00p03n01i03049"
      severity NOTE;
    assert ( C = '1' )
      report "***FAILED TEST: c12s02b04x00p03n01i03049 - A port of mode in assumes the value of the default expression when there is no associated signal."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s02b04x00p03n01i03049arch;
