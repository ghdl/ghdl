
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
-- $Id: tc402.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p11n01i00402ent IS
END c03s02b01x01p11n01i00402ent;

ARCHITECTURE c03s02b01x01p11n01i00402arch OF c03s02b01x01p11n01i00402ent IS
  function WR_OR(Input : BIT_VECTOR) return BIT is
  begin
    for I in Input'Range loop
      if Input(I) = '1' then
        return '1';
      end if;
    end loop;
  end;
BEGIN
  TESTING: PROCESS
    variable V1 : BIT_VECTOR(0 to 3) := "0101" ;
    variable V2 : BIT;
  BEGIN
    V2 := WR_OR(V1) ;  -- No_failure_here
    assert NOT(V2 = '1')
      report "***PASSED TEST: c03s02b01x01p11n01i00402"
      severity NOTE;
    assert ( V2 = '1' )
      report "***FAILED TEST: c03s02b01x01p11n01i00402 - For a formal parameter of a subprogram that is of an unconstrained array type, the index ranges are obtained from the corresponding association element in the applicable subprogram call."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p11n01i00402arch;
