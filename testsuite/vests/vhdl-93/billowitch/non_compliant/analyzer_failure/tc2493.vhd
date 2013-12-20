
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
-- $Id: tc2493.vhd,v 1.2 2001-10-26 16:30:19 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b03x00p04n01i02493ent IS
END c07s03b03x00p04n01i02493ent;

ARCHITECTURE c07s03b03x00p04n01i02493arch OF c07s03b03x00p04n01i02493ent IS

BEGIN
  TESTING: PROCESS
    type     index_values is (one, two, three);
    type     ucarr is array  (index_values range <>) of Boolean;
    subtype  carr is ucarr   (index_values'low to index_values'high);
    function f1 (i : integer) return carr is
    begin
      return (index_values'LOW => TRUE, others => False);
    end f1;
    variable V1 : CARR;
    variable I1 : Integer := 10;
  BEGIN
    V1 := f1(I1,10) ;        -- Failure_here
    assert FALSE 
      report "***FAILED TEST: c07s03b03x00p04n01i02493 - Each formal parameter of a function should have exactly one actual parameter associated with it in a function call."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b03x00p04n01i02493arch;
