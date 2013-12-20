
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
-- $Id: tc405.vhd,v 1.2 2001-10-26 16:30:26 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p19n01i00405ent IS
END c03s02b01x01p19n01i00405ent;

ARCHITECTURE c03s02b01x01p19n01i00405arch OF c03s02b01x01p19n01i00405ent IS
  type MEM is array (positive range <>) of BIT;
  type ME1 is array (positive range <>) of Integer;
  subtype ME2 is ME1(1 to 3);
  subtype M1 is MEM (1 to 5);
  function WR_OR(Input : ME1) return M1 is
  begin
    for I in Input'Range loop
      if Input(I) = 2 then
        return "11111" ;
      end if;
    end loop;
  end WR_OR;
  procedure F2 (X1 : in MEM; WR_OR: out M1) is
  begin
  end F2;
BEGIN
  TESTING: PROCESS
    variable V1 :ME2 := (20, 30, 40, 50);
  BEGIN
    F2(WR_OR(V1),WR_OR(V1)) ;  -- failure_here
    wait for 10 ns;
    assert FALSE 
      report "***FAILED TEST: c03s02b01x01p19n01i00405 - For an interface object of mode out, buffer, inout, or linkage, if the formal part includes a type conversion function, then the parameter subtype of that function must be a constrained array subtype."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p19n01i00405arch;
