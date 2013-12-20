
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
-- $Id: tc98.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x00p29n02i00098ent_a IS
  GENERIC ( gen_in : IN INTEGER );
  PORT ( prt_in : IN INTEGER );
  
  ATTRIBUTE attr1 : INTEGER;
  ATTRIBUTE attr1 OF gen_in : CONSTANT IS 100;
  ATTRIBUTE attr1 OF prt_in : SIGNAL   IS 200;
END c04s03b02x00p29n02i00098ent_a;

ARCHITECTURE c04s03b02x00p29n02i00098arch_a OF c04s03b02x00p29n02i00098ent_a IS
  
BEGIN
  PROCESS
  BEGIN
    ASSERT gen_in'attr1 = 100 REPORT "ERROR: Bad value for gen_in'attr1" SEVERITY FAILURE;
    ASSERT prt_in'attr1 = 200 REPORT "ERROR: Bad value for prt_in'attr1" SEVERITY FAILURE;
    assert NOT(gen_in'attr1 = 100 and prt_in'attr1 = 200) 
      report "***PASSED TEST: c04s03b02x00p29n02i00098" 
      severity NOTE;
    assert (gen_in'attr1 = 100 and prt_in'attr1 = 200) 
      report "***FAILED TEST: c04s03b02x00p29n02i00098 - Attribute reading fail."
      severity ERROR;
    wait;
  END PROCESS;
END c04s03b02x00p29n02i00098arch_a;



ENTITY c04s03b02x00p29n02i00098ent IS
END c04s03b02x00p29n02i00098ent;

ARCHITECTURE c04s03b02x00p29n02i00098arch OF c04s03b02x00p29n02i00098ent IS
  COMPONENT c04s03b02x00p29n02i00098ent_a
    GENERIC ( gen_in : IN INTEGER );
    PORT ( prt_in : IN INTEGER );
  END COMPONENT;
  FOR cmp1 : c04s03b02x00p29n02i00098ent_a USE ENTITY work.c04s03b02x00p29n02i00098ent_a(c04s03b02x00p29n02i00098arch_a); 
  
  SIGNAL s : INTEGER;
BEGIN

  cmp1 : c04s03b02x00p29n02i00098ent_a
    GENERIC MAP ( 0 )
    PORT MAP ( s );

END c04s03b02x00p29n02i00098arch;
