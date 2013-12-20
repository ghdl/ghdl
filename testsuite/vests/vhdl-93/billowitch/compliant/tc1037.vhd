
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
-- $Id: tc1037.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c06s04b00x00p02n01i01037pkg is
  type    THREE    is range 1 to 3;
  type    A1    is array (THREE) of BOOLEAN;
  function    Af1    (g : integer) return A1;
end c06s04b00x00p02n01i01037pkg;

package body c06s04b00x00p02n01i01037pkg is
  function Af1 (g : integer) return A1 is
    variable vaf1 : A1;
  begin
    return Vaf1;
  end Af1;
end c06s04b00x00p02n01i01037pkg;

use work.c06s04b00x00p02n01i01037pkg.all;
ENTITY c06s04b00x00p02n01i01037ent IS
  generic (g : integer := 2);
  port (PT: BOOLEAN) ;
  attribute AT1 : A1;
  attribute AT1 of PT : signal is Af1(g) ;
END c06s04b00x00p02n01i01037ent;

ARCHITECTURE c06s04b00x00p02n01i01037arch OF c06s04b00x00p02n01i01037ent IS

BEGIN
  TESTING: PROCESS
    variable V: BOOLEAN;
  BEGIN
    V := PT'AT1(1);
    assert NOT(V=false) 
      report "***PASSED TEST: c06s04b00x00p02n01i01037"
      severity NOTE;
    assert (V=false) 
      report "***FAILED TEST: c06s04b00x00p02n01i01037 - Indexed name be an attribute name test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p02n01i01037arch;
