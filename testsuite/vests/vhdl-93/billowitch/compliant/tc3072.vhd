
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
-- $Id: tc3072.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c12s06b02x00p06n01i03072pkg is
  type   natural_cons_vector is array (15 downto 0) of natural;
  constant C19 : natural_cons_vector := (others => 3);
end c12s06b02x00p06n01i03072pkg;

use work.c12s06b02x00p06n01i03072pkg.all;
ENTITY c12s06b02x00p06n01i03072ent_a IS
  PORT
    (
      F1:  OUT  integer ;
      F3:  IN   natural_cons_vector;
      FF:  OUT  integer := 0
      );
END c12s06b02x00p06n01i03072ent_a;

ARCHITECTURE c12s06b02x00p06n01i03072arch_a OF c12s06b02x00p06n01i03072ent_a IS

BEGIN
  TESTING: PROCESS
  begin
    F1 <= 3;
    wait for 0 ns;
    assert F3'active = true
      report"no activity on F3 when there is activity on actual" 
      severity failure;
    if (not(F3'active = true)) then
      F1 <= 11;
    end if;
    assert F3(0)'active = true
      report"no activity on F3 when there is activity on actual" 
      severity failure;
    if (not(F3(0)'active = true)) then
      F1 <= 11;
    end if;
    assert F3(15)'active = true
      report"no activity on F3 when there is activity on actual" 
      severity failure;
    if (not(F3(15)'active = true)) then
      F1 <= 11;
    end if;
    wait;
  END PROCESS;

END c12s06b02x00p06n01i03072arch_a;


use work.c12s06b02x00p06n01i03072pkg.all;
ENTITY c12s06b02x00p06n01i03072ent IS
END c12s06b02x00p06n01i03072ent;

ARCHITECTURE c12s06b02x00p06n01i03072arch OF c12s06b02x00p06n01i03072ent IS
  function scalar_complex(s : integer) return natural_cons_vector is
  begin
    return C19;
  end scalar_complex;
  component model
    PORT
      (    
        F1:  OUT  integer;
        F3:  IN   natural_cons_vector;
        FF:  OUT  integer
        );
  end component;
  for T1 : model use entity work.c12s06b02x00p06n01i03072ent_a(c12s06b02x00p06n01i03072arch_a);
  signal S1 : natural_cons_vector;
  signal S3 : integer;
  signal SS : integer := 0;
BEGIN
  T1: model
    port map (
      scalar_complex(F1) => S1,
      F3 => scalar_complex(S3),
      FF => SS
      );
  TESTING: PROCESS
  BEGIN

    S3 <= 3;
    wait for 0 ns;
    assert S1'active = true
      report"no activity on F3 when there is activity on actual" 
      severity failure;
    assert S1(0)'active = true
      report"no activity on F3 when there is activity on actual" 
      severity failure;
    assert S1(15)'active = true
      report"no activity on F3 when there is activity on actual" 
      severity failure;

    assert NOT(S1'active = true and S1(0)'active = true and S1(15)'active = true and SS = 0)
      report "***PASSED TEST: c12s06b02x00p06n01i03072"
      severity NOTE;
    assert (S1'active = true and S1(0)'active = true and S1(15)'active = true and SS = 0)
      report "***FAILED TEST: c12s06b02x00p06n01i03072 - Not every scalar subelement is active if the source itself is active."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c12s06b02x00p06n01i03072arch;
