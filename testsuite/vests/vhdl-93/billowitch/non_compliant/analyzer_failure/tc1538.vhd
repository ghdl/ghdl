
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
-- $Id: tc1538.vhd,v 1.2 2001-10-26 16:30:11 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s09b00x00p09n05i01538ent IS
END c08s09b00x00p09n05i01538ent;

ARCHITECTURE c08s09b00x00p09n05i01538arch OF c08s09b00x00p09n05i01538ent IS
  procedure copy ( variable v_in : in integer := 0;
                   variable v_out : inout integer
                   ) is
  begin
    v_out := v_in;
  end copy;
BEGIN
  TESTING: PROCESS
  BEGIN

    L1: for i in 4 to 5 loop
      copy (6, i);                  -- illegal: 2nd param is mode "inout"
    end loop L1;

    assert FALSE 
      report "***FAILED TEST: c08s09b00x00p09n05i01538 - A loop parameter can not be an actual corresponding to a formal of mode inout "
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s09b00x00p09n05i01538arch;
