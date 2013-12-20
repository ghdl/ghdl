
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
-- $Id: tc1472.vhd,v 1.2 2001-10-26 16:30:10 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c08s08b00x00p03n01i01472ent IS
END c08s08b00x00p03n01i01472ent;

ARCHITECTURE c08s08b00x00p03n01i01472arch OF c08s08b00x00p03n01i01472ent IS

  type primary is (blue, red, yellow);

  --
  -- Test that the '<=' can not be used
  --
  function color_to_int ( constant color : primary
                          ) return integer is
  begin
    case color is
      when others <=                -- should be '=>'
                                    return primary'pos(color);
    end case;
  end color_to_int;

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c08s08b00x00p03n01i01472 - Signal assignment operator can not be used as case alternative delimiter." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s08b00x00p03n01i01472arch;
