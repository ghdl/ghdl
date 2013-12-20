
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
-- $Id: tc3054.vhd,v 1.2 2001-10-26 16:29:51 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c12s03b00x00p07n03i03054ent IS
END c12s03b00x00p07n03i03054ent;

ARCHITECTURE c12s03b00x00p07n03i03054arch OF c12s03b00x00p07n03i03054ent IS

BEGIN
  bl1: block
    signal si : integer := 3;
    function int (signal sf : in integer) return integer is
      constant err:integer := sf;
    begin
      return err;
    end;
  begin
    TESTING: PROCESS
    BEGIN
      wait for 5 ns;
      assert NOT( si = int(si) )
        report "***PASSED TEST: c12s03b00x00p07n03i03054"
        severity NOTE;
      assert ( si = int(si) )
        report "***FAILED TEST: c12s03b00x00p07n03i03054 - Name of a signal used in the declarative part of a subprogram test failed."
        severity ERROR;
      wait;
    END PROCESS TESTING;
  end block;

END c12s03b00x00p07n03i03054arch;
