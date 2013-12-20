
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
-- $Id: tc2771.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c13s08b00x00p01n01i02771pkg is
  function--This is a valid comment.
    F1 return BOOLEAN;
  function F2 return BOOLEAN;
end c13s08b00x00p01n01i02771pkg;

package body c13s08b00x00p01n01i02771pkg is
  function--This is a valid comment.
    F1 return BOOLEAN is

  begin
    return --This comment occurs within a statement!
      FALSE-- Comments can occur anywhere and need not be
      --           preceded by a blank
      ;
  end F1;

  function F2 return BOOLEAN is
    type TYP_1 is range 1 to 10;
    variable V1--This is all one comment--not two  --  or more!
              : TYP_1 := 2;
  begin
    assert TRUE
      report "--This is not a comment--";
    return FALSE;
  end F2;
end c13s08b00x00p01n01i02771pkg;

ENTITY c13s08b00x00p01n01i02771ent IS
  port (PT:BOOLEAN) ;
  --This is a NULL entity
END c13s08b00x00p01n01i02771ent;

ARCHITECTURE c13s08b00x00p01n01i02771arch OF c13s08b00x00p01n01i02771ent IS

--
--(that was a blank comment)

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c13s08b00x00p01n01i02771" 
      severity NOTE;
    wait;
  END PROCESS TESTING
--that wasn't so quick!
    ;--semicolon


END c13s08b00x00p01n01i02771arch; --architecture A ("A comment can appear on any line of a VHDL description.")
