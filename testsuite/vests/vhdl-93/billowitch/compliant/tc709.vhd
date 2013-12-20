
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
-- $Id: tc709.vhd,v 1.3 2001-10-29 02:12:46 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------


--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Tue Nov  5 16:38:09 1996  --
--                 ****************************               --



--                 ****************************                   --
-- Reversed to VHDL 87 by reverse87.pl - Tue Nov  5 11:26:45 1996  --
--                 ****************************                    --



--                 ****************************               --
-- Ported to VHDL 93 by port93.pl - Mon Nov  4 17:36:47 1996  --
--                 ****************************               --


ENTITY c03s04b01x00p23n01i00709ent IS
END c03s04b01x00p23n01i00709ent;

ARCHITECTURE c03s04b01x00p23n01i00709arch OF c03s04b01x00p23n01i00709ent IS
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
    -- Declare the actual file to read.
    file FILEV : ft1 open read_mode is "iofile.59";
    
    -- Declare a variable into which we will read.
    variable VAR : t1;
    variable k   : integer      := 0;   
  BEGIN
    -- Read in the file.
    for I in 1 to NumofStrings loop
      if (ENDFILE( FILEV ) /= FALSE) then
        k := 1;
      end if;
      assert( (ENDFILE( FILEV ) = FALSE) )
        report "Hit the end of file too soon.";
      READ( FILEV,VAR );
      if ((VAR.number /= i) or (VAR.string /= string_array(i))) then
        k := 1;
      end if;
    end loop;
    
    -- Verify that we are at the end.
    if (ENDFILE( FILEV ) /= TRUE) then
      k := 1;
    end if;
    assert( ENDFILE( FILEV ) = TRUE )
      report "Have not reached end of file yet."
      severity ERROR;   

    assert NOT( k = 0 )
      report "***PASSED TEST: c03s04b01x00p23n01i00709"
      severity NOTE;
    assert( k = 0 )
      report "***FAILED TEST: c03s04b01x00p23n01i00709 - The variables don't equal the constants." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s04b01x00p23n01i00709arch;
