
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
-- $Id: tc792.vhd,v 1.2 2001-10-26 16:30:00 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s01b02x00p03n01i00792pkg is
  constant k : integer := 5;
  function wired_and (sig : bit_vector) return bit;
end c01s01b02x00p03n01i00792pkg;

package body c01s01b02x00p03n01i00792pkg is
  function wired_and (sig : bit_vector) return bit is
  begin
    return '0';
  end wired_and;
end c01s01b02x00p03n01i00792pkg;


ENTITY c01s01b02x00p03n01i00792ent_1 IS
  GENERIC (CONSTANT a : bit);
  ALIAS alias_identifier : bit IS a ;
END    c01s01b02x00p03n01i00792ent_1 ;

ENTITY c01s01b02x00p03n01i00792ent_2 IS
  GENERIC (CONSTANT a : bit);
  ATTRIBUTE my_name : integer;
END    c01s01b02x00p03n01i00792ent_2 ;

ENTITY c01s01b02x00p03n01i00792ent_4 IS
  GENERIC (CONSTANT a : bit);
  USE work.c01s01b02x00p03n01i00792pkg.ALL;
END    c01s01b02x00p03n01i00792ent_4 ;

use work.c01s01b02x00p03n01i00792pkg.all;
ENTITY c01s01b02x00p03n01i00792ent_5 IS
  port (signal a : in wired_and bit bus);
  DISCONNECT a:bit AFTER 100 ns;
END    c01s01b02x00p03n01i00792ent_5 ;

--------------------------------
ENTITY c01s01b02x00p03n01i00792ent IS
END c01s01b02x00p03n01i00792ent;

ARCHITECTURE c01s01b02x00p03n01i00792arch OF c01s01b02x00p03n01i00792ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c01s01b02x00p03n01i00792" 
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c01s01b02x00p03n01i00792arch;
