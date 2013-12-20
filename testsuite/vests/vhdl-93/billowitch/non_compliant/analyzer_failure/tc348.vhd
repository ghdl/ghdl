
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
-- $Id: tc348.vhd,v 1.2 2001-10-26 16:30:25 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x00p15n01i00348ent IS
END c03s02b01x00p15n01i00348ent;

ARCHITECTURE c03s02b01x00p15n01i00348arch OF c03s02b01x00p15n01i00348ent IS

BEGIN
  TESTING: PROCESS
    function WIRED_OR ( Inputs: BIT_VECTOR ) return BIT is
      constant Floatvalue : BIT := '0' ;
    begin
      if Inputs'Length = 0 then
        -- this is a bus whose drivers are all off.
        return FloatValue ;
      else
        for I in Inputs'Range loop
          if Inputs(I) = '1' then
            return '1' ;
          end if ;
        end loop ;
        return '0' ;
      end if ;
    end;
    type bad_array_type is array (WIRED_OR INTEGER range 12 to 22) of BIT; 
    -- Failure_here         
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c03s02b01x00p15n01i00348 - Resolution function cannot be present." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x00p15n01i00348arch;
