
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
-- $Id: tc945.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s01b00x00p09n01i00945ent IS
END c06s01b00x00p09n01i00945ent;

ARCHITECTURE c06s01b00x00p09n01i00945arch OF c06s01b00x00p09n01i00945ent IS

BEGIN
  TESTING: PROCESS
    function "+" (a, b:in integer) return bit is
      variable c: bit;
      variable d: integer := 0;
    begin
      d := a + b + "+".d; -- function call can be used as a prefix.
      if (d > 0) then
        c := '0';
      end if;
      if (d < 0) then
        c := '1';
      end if;
      return c; 
    end;
    variable k : bit;
  BEGIN
    k := "+"(1,2);
    assert NOT(k='0') 
      report "***PASSED TEST: c06s01b00x00p09n01i00945" 
      severity NOTE;
    assert (k='0') 
      report "***FAILED TEST: c06s01b00x00p09n01i00945 - Prefix can only be a name or a function_call."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s01b00x00p09n01i00945arch;
