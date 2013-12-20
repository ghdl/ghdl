
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
-- $Id: tc3110.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s01b00x00p34n01i03110ent_a IS
  PORT    ( Y : IN   BIT ;
            Z : OUT   INTEGER );
  ATTRIBUTE    A : INTEGER;
  ATTRIBUTE    A OF Y : SIGNAL IS 1;
END c05s01b00x00p34n01i03110ent_a;

ARCHITECTURE c05s01b00x00p34n01i03110arch_a OF c05s01b00x00p34n01i03110ent_a IS

BEGIN
  PROCESS
  BEGIN
    ASSERT Y'A = 1 
      REPORT "ERROR: Bad value for Y'A" SEVERITY FAILURE;
    if (Y'A = 1) then
      Z <= 100;
    end if;
    WAIT;
  END PROCESS;
END c05s01b00x00p34n01i03110arch_a;



ENTITY c05s01b00x00p34n01i03110ent IS
  ATTRIBUTE A : INTEGER;
END c05s01b00x00p34n01i03110ent;

ARCHITECTURE c05s01b00x00p34n01i03110arch OF c05s01b00x00p34n01i03110ent IS
  COMPONENT c05s01b00x00p34n01i03110ent_a
    PORT (    Y : IN   BIT ;
              Z : OUT   INTEGER );
  END COMPONENT;
  for all : c05s01b00x00p34n01i03110ent_a use entity work.c05s01b00x00p34n01i03110ent_a(c05s01b00x00p34n01i03110arch_a); 

  SIGNAL       X : BIT;
  SIGNAL       XX: INTEGER;
  ATTRIBUTE    A OF X : SIGNAL IS 2;
BEGIN

  inst1 : c05s01b00x00p34n01i03110ent_a PORT MAP ( Y => X , Z => XX );

  TESTING: PROCESS
  BEGIN
    wait for 5 ns;
    assert NOT( X'A = 2 and XX = 100 )
      report "***PASSED TEST: c05s01b00x00p34n01i03110"
      severity NOTE;
    assert ( X'A = 2 and XX = 100 )
      report "***FAILED TEST: c05s01b00x00p34n01i03110 - User defined attribute represent local information only."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s01b00x00p34n01i03110arch;
