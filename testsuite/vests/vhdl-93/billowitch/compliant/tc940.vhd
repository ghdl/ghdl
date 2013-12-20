
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
-- $Id: tc940.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c10s05b00x00p03n02i00940pkg1 is
  function F1(B:in integer) return integer;
end c10s05b00x00p03n02i00940pkg1;

package body c10s05b00x00p03n02i00940pkg1 is
  function  F1(B:in integer) return integer is
  begin
    return 1;
  end;
end c10s05b00x00p03n02i00940pkg1;


package c10s05b00x00p03n02i00940pkg2 is
  function F1(B:in bit) return boolean;
end c10s05b00x00p03n02i00940pkg2;

package body c10s05b00x00p03n02i00940pkg2 is
  function  F1(B:in bit) return boolean is
  begin
    return false;
  end;
end c10s05b00x00p03n02i00940pkg2;

use work.c10s05b00x00p03n02i00940pkg1.all, work.c10s05b00x00p03n02i00940pkg2.all;
ENTITY c10s05b00x00p03n02i00940ent IS
END c10s05b00x00p03n02i00940ent;

ARCHITECTURE c10s05b00x00p03n02i00940arch OF c10s05b00x00p03n02i00940ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    if (F1('1') = F1('0')) then  -- No_Failure_here
      assert FALSE 
        report "***PASSED TEST: c10s05b00x00p03n02i00940" 
        severity NOTE;
    else
      assert FALSE 
        report "***FAILED TEST: c10s05b00x00p03n02i00940 - A single interpretation of each constituent of the innermost complete context is not an error." 
        severity ERROR;
    end if;
    wait;
  END PROCESS TESTING;

END c10s05b00x00p03n02i00940arch;
