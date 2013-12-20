
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
-- $Id: tc1038.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c06s04b00x00p02n01i01038pkg is
  type    THREE    is range 1 to 3;
  type    A2    is array (THREE, THREE) of BOOLEAN;
  function    Af2    (g : integer) return A2;
end c06s04b00x00p02n01i01038pkg;

package body c06s04b00x00p02n01i01038pkg is
  function Af2 (g : integer) return A2 is
    variable vaf1 : A2;
  begin
    return Vaf1;
  end Af2;
end c06s04b00x00p02n01i01038pkg;

use work.c06s04b00x00p02n01i01038pkg.all;
ENTITY c06s04b00x00p02n01i01038ent IS
  generic (g : integer := 2);
  port (PT: BOOLEAN) ;
  attribute AT2 : A2;
  attribute AT2 of PT : signal is Af2(g) ;
END c06s04b00x00p02n01i01038ent;

ARCHITECTURE c06s04b00x00p02n01i01038arch OF c06s04b00x00p02n01i01038ent IS

BEGIN
  TESTING: PROCESS
    variable V: BOOLEAN;
  BEGIN
    V := PT'AT2(2, 3);
    assert NOT(V=false) 
      report "***PASSED TEST: c06s04b00x00p02n01i01038" 
      severity NOTE;
    assert (V=false) 
      report "***FAILED TEST: c06s04b00x00p02n01i01038 - Indexed name be an attribute name test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s04b00x00p02n01i01038arch;
