
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
-- $Id: tc1434.vhd,v 1.2 2001-10-26 16:30:10 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s07b00x00p02n01i01434ent IS
END c08s07b00x00p02n01i01434ent;

ARCHITECTURE c08s07b00x00p02n01i01434arch OF c08s07b00x00p02n01i01434ent IS
begin

  TEST_PROCESS:  process
    variable I : INTEGER := 47;
  begin
    if  (I /= 47) the
      NULL;   
  else if (I = 47) then
         NULL;
       end if;
       assert FALSE 
         report "***FAILED TEST: c08s07b00x00p02n01i01434 - reserved word 'then' misspelled"
         severity ERROR;
       wait;
  end process TEST_PROCESS;

END c08s07b00x00p02n01i01434arch;
