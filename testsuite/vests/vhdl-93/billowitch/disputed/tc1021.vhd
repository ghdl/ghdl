
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
-- $Id: tc1021.vhd,v 1.2 2001-10-26 16:30:03 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p10n01i01021ent IS
END c06s03b00x00p10n01i01021ent;

ARCHITECTURE c06s03b00x00p10n01i01021arch OF c06s03b00x00p10n01i01021ent IS
BEGIN
  B1:Block
    signal s1 : BIT;
  begin
    TESTING: PROCESS
    BEGIN
      wait for 1 ns;
    END PROCESS TESTING;

    B2:Block
      signal s2 : BIT;
    begin
      TEST : PROCESS
      BEGIN
        s2 <= B1.s1;
        wait for 2 ns;
        assert NOT(s2='0')
          report "***PASSED TEST: c06s03b00x00p10n01i01021" 
          severity NOTE;
        assert (s2='0')
          report "***FAILED TEST: c06s03b00x00p10n01i01021 - Entity declaration does not occur in construct specifed by the prefix." 
          severity ERROR;
      END PROCESS TEST;
    end block B2;
  end block B1;

END c06s03b00x00p10n01i01021arch;
