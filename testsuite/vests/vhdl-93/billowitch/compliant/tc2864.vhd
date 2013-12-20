
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
-- $Id: tc2864.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b00x00p03n01i02864ent IS
END c02s01b00x00p03n01i02864ent;

ARCHITECTURE c02s01b00x00p03n01i02864arch OF c02s01b00x00p03n01i02864ent IS

BEGIN
  TESTING: PROCESS
    function greater (i,l:time) return boolean;
    function greater (i,l:time) return boolean is
    begin
      if i > l then
        return TRUE;
      else
        return FALSE;
      end if;
    end greater;
    variable result : boolean;   
  BEGIN
    result := greater (10 ns, 5 ns);
    assert NOT( result = true )
      report "***PASSED TEST: c02s01b00x00p03n01i02864"
      severity NOTE;
    assert ( result = true )
      report "***FAILED TEST: c02s01b00x00p03n01i02864 - Funcation call syntax test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b00x00p03n01i02864arch;
