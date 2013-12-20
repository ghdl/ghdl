
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
-- $Id: tc1018.vhd,v 1.2 2001-10-26 16:30:05 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p10n01i01018ent IS
  port (p : in bit);
END c06s03b00x00p10n01i01018ent;

ARCHITECTURE c06s03b00x00p10n01i01018arch OF c06s03b00x00p10n01i01018ent IS

BEGIN
  b1 : block
    type chars is ('a','b','c');
    signal bs1 : BIT;
  begin
    B2: block
      signal bs2 : BIT;
    begin
      process
      begin
        NULL;
      end process;
      b3 : block
        signal bs3a : BIT;
        signal bs3b : BIT;
      begin
        bs3b <= B1.bs2;
                                        -- ERROR: Entity defined by selected
                                        -- suffix not declared within construct
                                        -- denoted by selected prefix.
      end block b3;
    end block B2;
  end block b1;

  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c06s03b00x00p10n01i01018 - Entity declaration does not occur in construct specifed by the prefix." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p10n01i01018arch;
