
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
-- $Id: tc3064.vhd,v 1.2 2001-10-26 16:30:25 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s03b02x02p05n01i03064ent IS
  port(con : in BIT := '1'; clk : out BIT);
END c12s03b02x02p05n01i03064ent;

ARCHITECTURE c12s03b02x02p05n01i03064arch OF c12s03b02x02p05n01i03064ent IS

BEGIN
  TESTING: PROCESS
  begin
    clk <= con;
    wait;
  END PROCESS TESTING;

END c12s03b02x02p05n01i03064arch_a;


ENTITY c12s03b02x02p05n01i03064ent IS
  port (C : out bit);
END c12s03b02x02p05n01i03064ent;

ARCHITECTURE c12s03b02x02p05n01i03064arch OF c12s03b02x02p05n01i03064ent IS
  component c12s03b02x02p05n01i03064ent_aa 
    port(con : in bit:='1'; clk : out bit);
  end component;
  for all: c12s03b02x02p05n01i03064ent_aa use entity work.fail(c12s03b02x02p05n01i03064arch_a);       -- Failure_here
BEGIN
  T1: test port map(open,C);
  TESTING: PROCESS
  BEGIN
    assert FAILED
      report "***FAILED TEST: c12s03b02x02p05n01i03064 - Entity declaration and the corresponding body implied by the binding indication do not exist within the specified library."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s03b02x02p05n01i03064arch;
