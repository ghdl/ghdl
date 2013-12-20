
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
-- $Id: tc889.vhd,v 1.2 2001-10-26 16:30:01 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

PACKAGE c10s02b00x00p12n01i00889pkg IS
  CONSTANT zero : INTEGER := 0;
END c10s02b00x00p12n01i00889pkg;


USE WORK.c10s02b00x00p12n01i00889pkg.all;
ENTITY c10s02b00x00p12n01i00889ent_a IS
  GENERIC ( I : INTEGER );
END c10s02b00x00p12n01i00889ent_a;


ARCHITECTURE c10s02b00x00p12n01i00889arch_a OF c10s02b00x00p12n01i00889ent_a IS

BEGIN
  PROCESS
  BEGIN
    assert NOT( I=0 )
      report "***PASSED TEST: c10s02b00x00p12n01i00889"
      severity NOTE;
    assert ( I=0 )
      report "***FAILED TEST: c10s02b00x00p12n01i00889"
      severity ERROR;
    wait;
  END PROCESS;
END;

USE WORK.c10s02b00x00p12n01i00889pkg.all;
ENTITY c10s02b00x00p12n01i00889ent IS
END c10s02b00x00p12n01i00889ent;

ARCHITECTURE c10s02b00x00p12n01i00889arch OF c10s02b00x00p12n01i00889ent IS

  COMPONENT c10s02b00x00p12n01i00889ent_a
  END COMPONENT;

BEGIN
  comp1 : c10s02b00x00p12n01i00889ent_a;

END c10s02b00x00p12n01i00889arch;


CONFIGURATION c10s02b00x00p12n01i00889cfg OF c10s02b00x00p12n01i00889ent IS
  FOR c10s02b00x00p12n01i00889arch
    FOR comp1 : c10s02b00x00p12n01i00889ent_a
      USE ENTITY WORK.c10s02b00x00p12n01i00889ent_a(c10s02b00x00p12n01i00889arch_a) GENERIC MAP ( zero );
    END FOR;
  END FOR;
END c10s02b00x00p12n01i00889cfg;
