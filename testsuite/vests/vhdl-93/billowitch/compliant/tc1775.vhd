
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
-- $Id: tc1775.vhd,v 1.2 2001-10-26 16:29:43 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c09s05b02x00p11n01i01775ent IS
END c09s05b02x00p11n01i01775ent;

ARCHITECTURE c09s05b02x00p11n01i01775arch OF c09s05b02x00p11n01i01775ent IS
  signal i : integer := 21;
  signal j : boolean ;
BEGIN

  with i select
    j <= transport
    TRUE  when 1  to 19,  -- No_failure_here
                              -- Valid expression for a choice
    FALSE when 20 to 29,
    TRUE  when 30 to 49,
    FALSE when others;

  TESTING: PROCESS(j)
  BEGIN
    assert NOT(j = FALSE) 
      report "***PASSED TEST: c09s05b02x00p11n01i01775" 
      severity NOTE;
    assert (j = FALSE) 
      report "***FAILED TEST: c09s05b02x00p11n01i01775 - Each value of the type of the select expression should be represented once and exactly once." 
      severity ERROR;
  END PROCESS TESTING;

END c09s05b02x00p11n01i01775arch;
