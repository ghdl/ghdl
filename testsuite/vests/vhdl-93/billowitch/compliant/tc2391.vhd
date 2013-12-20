
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
-- $Id: tc2391.vhd,v 1.2 2001-10-26 16:29:47 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s03b02x00p07n01i02391ent IS
END c07s03b02x00p07n01i02391ent;

ARCHITECTURE c07s03b02x00p07n01i02391arch OF c07s03b02x00p07n01i02391ent IS
  type ARRAY_TYPE is array (INTEGER range <>) of BOOLEAN;
  type RECORD_TYPE is record
                        E1,E2,E3,E4,E5 : BOOLEAN;
                      end record;
  signal S3 : ARRAY_TYPE(1 to 5);
BEGIN
  TESTING: PROCESS
  BEGIN
    S3 <= ( 5 => TRUE, 4|2 downto 1 => TRUE, 3 => TRUE);
    -- named associations may appear in any order.
    wait for 1 ns;
    assert NOT(S3(1)=TRUE and S3(2)=TRUE and S3(3)=TRUE and S3(4)=TRUE and S3(5)=TRUE) 
      report "***PASSED TEST: c07s03b02x00p07n01i02391" 
      severity NOTE;
    assert (S3(1)=TRUE and S3(2)=TRUE and S3(3)=TRUE and S3(4)=TRUE and S3(5)=TRUE) 
      report "***FAILED TEST: c07s03b02x00p07n01i02391 - Both named and positional associations can be used in the same aggregate." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s03b02x00p07n01i02391arch;
