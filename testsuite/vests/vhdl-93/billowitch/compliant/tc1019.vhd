
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
-- $Id: tc1019.vhd,v 1.2 2001-10-26 16:29:38 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p10n01i01019ent IS
  port (p : in bit);
END c06s03b00x00p10n01i01019ent;

ARCHITECTURE c06s03b00x00p10n01i01019arch OF c06s03b00x00p10n01i01019ent IS

BEGIN
  B1:Block
    type chars is ('a', 'b', 'c', 'd', 'e');
  begin
    TESTING: PROCESS
      variable c           : chars;
      variable All_done    : boolean;
    BEGIN
      L1 : for LL1 in TRUE downto FALSE  loop
        NULL; 
        if L1.LL1 then       -- Selected prefix is loop,
                                       -- suffix is identifier that
                                                 -- refers to loop iteration id.
          All_done := True;
        end if;
      end loop L1;
      assert NOT(All_done=TRUE) 
        report "***PASSED TEST: c06s03b00x00p10n01i01019"
        severity NOTE;
      assert (All_done=TRUE) 
        report "***FAILED TEST: c06s03b00x00p10n01i01019 - Entity declaration does not occur in construct specifed by the prefix." 
        severity ERROR;
      wait;
    END PROCESS TESTING;
  end block B1;

END c06s03b00x00p10n01i01019arch;
