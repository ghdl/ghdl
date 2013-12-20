
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
-- $Id: tc3197.vhd,v 1.3 2001-10-29 02:12:45 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library std;
use std.TEXTIO.all;
ENTITY c14s03b00x00p42n01i03197ent IS
END c14s03b00x00p42n01i03197ent;

ARCHITECTURE c14s03b00x00p42n01i03197arch OF c14s03b00x00p42n01i03197ent IS

BEGIN
  TESTING: PROCESS
    file F      : TEXT open read_mode is "iofile.10";
    variable    L      : LINE;
    variable   vboolean   : boolean;
    variable   fail      : integer := 0;
  BEGIN
    for I in 1 to 100 loop
      READLINE   (F, L);
      READ       (L, vboolean);
      if (vboolean /= TRUE) then
        fail := 1;
      end if;
    end loop;
    assert NOT(fail = 0) 
      report "***PASSED TEST: c14s03b00x00p42n01i03197"
      severity NOTE;
    assert (fail = 0)
      report "***FAILED TEST: c14s03b00x00p42n01i03197 - procedure READLINE for boolean TEXT file test failed, plese check s010112.vhd file also."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c14s03b00x00p42n01i03197arch;
