
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
-- $Id: tc1730.vhd,v 1.2 2001-10-26 16:30:12 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s03b00x00p02n01i01730ent IS
  port (signal AA,BB: in bit);
END c09s03b00x00p02n01i01730ent;

ARCHITECTURE c09s03b00x00p02n01i01730arch OF c09s03b00x00p02n01i01730ent IS

  procedure P1 (signal A,B: in bit; signal C: out bit) is
  begin
    C <= A and B;
  end;
  
  signal   CC : bit;
BEGIN

  PROC  P1 (AA,BB,CC);     -- Failure_here
  -- Colon is misssing   

  assert FALSE 
    report "***FAILED TEST: c09s03b00x00p02n01i01730 - Colon between the label and a procedure call statement is missing." 
    severity ERROR;

END c09s03b00x00p02n01i01730arch;
