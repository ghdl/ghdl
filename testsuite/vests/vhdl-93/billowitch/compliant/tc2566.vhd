
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
-- $Id: tc2566.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s04b02x00p15n01i02566ent_a IS
  GENERIC ( gen_prm : INTEGER );
END c07s04b02x00p15n01i02566ent_a;

ARCHITECTURE c07s04b02x00p15n01i02566arch_a OF c07s04b02x00p15n01i02566ent_a IS
  SIGNAL s : BIT;

  ATTRIBUTE attr1 : INTEGER;
  ATTRIBUTE attr1 OF s : SIGNAL IS gen_prm;
--
--      Usage of user-defined attribute as a globally-static expression
--
  CONSTANT c0 : INTEGER := s'attr1;
BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( c0 = gen_prm )
      report "***PASSED TEST: c07s04b02x00p15n01i02566"
      severity NOTE;
    assert ( c0 = gen_prm )
      report "***FAILED TEST: c07s04b02x00p15n01i02566 - Constant initialization to user-attribute value failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s04b02x00p15n01i02566arch_a;



ENTITY c07s04b02x00p15n01i02566ent IS
END c07s04b02x00p15n01i02566ent;

ARCHITECTURE c07s04b02x00p15n01i02566arch OF c07s04b02x00p15n01i02566ent IS

  COMPONENT c07s04b02x00p15n01i02566ent_a
    GENERIC ( gen_prm : INTEGER );
  END COMPONENT;

  for cmp : c07s04b02x00p15n01i02566ent_a use entity work.c07s04b02x00p15n01i02566ent_a(c07s04b02x00p15n01i02566arch_a);

BEGIN

  cmp : c07s04b02x00p15n01i02566ent_a generic map (123);
END c07s04b02x00p15n01i02566arch;
