
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
-- $Id: tc713.vhd,v 1.3 2001-10-29 02:12:46 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:38:09 1996  --
--                 ****************************               --



--                 ****************************                   --
-- Reversed to VHDL 87 by reverse87.pl - Tue Nov  5 11:26:46 1996  --
--                 ****************************                    --



--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Mon Nov  4 17:36:48 1996  --
--                 ****************************               --


ENTITY c03s04b01x00p24n01i00713ent IS
END c03s04b01x00p24n01i00713ent;

ARCHITECTURE c03s04b01x00p24n01i00713arch OF c03s04b01x00p24n01i00713ent IS
  -- Some constants...
  constant StringLength: INTEGER := 16;
  constant NumOfStrings: INTEGER := 5;
  
  -- Types...;
  subtype STR16 is STRING (1 to StringLength);
  type string_table is array (1 to NumOfStrings) of STR16;

  -- Objects...
  constant string_array: string_table :=
    (  "This is string 1"
       ,"__Hello  World__"
       ,"This is string " & "3"
       ,"_Bird is a word_"
       ,"_Goodbye (ciao)_"
       );

  type ft3 is file of STRING;
BEGIN
  TESTING: PROCESS
    -- Declare the actual file to write.
    file FILEV : ft3 open read_mode is "iofile.02";
    variable length   : INTEGER;
    variable str   : STR16;
    variable count    : integer := 0;
    variable ok   : integer := 1;
  BEGIN
    count := 1;
    while not endfile(FILEV) loop
      read(FILEV, str, length);
      if (length /= 16) then
        ok := 0;
      end if;
      if (str /= string_array(count)) then
        ok := 0;
      end if;
      assert str = string_array(count)
        report "'string' is incorrect"
        severity ERROR;
      count := count + 1;
    end loop;
    assert NOT(ok = 1)
      report "***PASSED TEST: c03s04b01x00p24n01i00713" 
      severity NOTE;
    assert (ok = 1)
      report "***FAILED TEST: c03s04b01x00p24n01i00713 - READ operation for unconstrained array type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p24n01i00713arch;
