
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
-- $Id: tc388.vhd,v 1.2 2001-10-26 16:29:53 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s02b01x01p04n03i00388ent IS
END c03s02b01x01p04n03i00388ent;

ARCHITECTURE c03s02b01x01p04n03i00388arch OF c03s02b01x01p04n03i00388ent IS
  type SQ_ARR is array(0 to 1, 0 to 2) of BIT;
BEGIN
  TESTING: PROCESS
    variable M2 :  SQ_ARR :=  (('0','1','1'),('1','0','0')); -- No_failure_here
  BEGIN
    assert NOT(    M2(0,0)='0' and
                   M2(0,1)='1' and
                   M2(0,2)='1' and
                   M2(1,0)='1' and
                   M2(1,1)='0' and
                   M2(1,2)='0')   
      report "***PASSED TEST: c03s02b01x01p04n03i00388"
      severity NOTE;
    assert (    M2(0,0)='0' and
                M2(0,1)='1' and
                M2(0,2)='1' and
                M2(1,0)='1' and
                M2(1,1)='0' and
                M2(1,2)='0')   
      report "***FAILED TEST: c03s02b01x01p04n03i00388 - An array value staisfies an index constraint if at each index position the array value and the index constrint have the same index range."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b01x01p04n03i00388arch;
