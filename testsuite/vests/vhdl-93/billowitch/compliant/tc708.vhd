
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
-- $Id: tc708.vhd,v 1.3 2001-10-29 02:12:46 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:38:08 1996  --
--                 ****************************               --



ENTITY c03s04b01x00p23n01i00708ent IS
END c03s04b01x00p23n01i00708ent;

ARCHITECTURE c03s04b01x00p23n01i00708arch OF c03s04b01x00p23n01i00708ent IS
  -- Some constants...
  constant StringLength: INTEGER := 16;
  constant NumOfStrings: INTEGER := 5;
  
  -- Types...;
  subtype STR16 is STRING (1 to StringLength);
  type t1 is record
               number:      NATURAL;
               string:      STR16;
             end record;

  type string_table is array (1 to NumOfStrings) of STR16;
  
  -- Objects...
  constant string_array: string_table :=
    ( "This is string 1"
      ,"__Hello  World__"
      ,"This is string " & "3"
      ,"_Bird is a word_"
      ,"_Goodbye (ciao)_"
      );

  type ft1 is file of t1;
BEGIN
  TESTING: PROCESS
    -- Declare the actual file to write.
    file FILEV : ft1 open write_mode is "iofile.59";

    -- Declare a variable.
    variable VAR : t1;
  BEGIN
    -- Write out the file.
    for I in 1 to NumOfStrings  loop
      VAR.number := i;
      VAR.string := string_array(i);
      write(FILEV, VAR);
    end loop;
    assert FALSE
      report "***PASSED TEST: c03s04b01x00p23n01i00708 - The output file will tested by test file s010440.vhd"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p23n01i00708arch;
