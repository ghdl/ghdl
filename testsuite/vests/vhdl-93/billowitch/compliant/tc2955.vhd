
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
-- $Id: tc2955.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s02b00x00p02n01i02955ent IS
  procedure greater (i:integer; res:out boolean);
  procedure greater (i:integer; res:out boolean) is
    type       mine    is (vero,falso);
    subtype    digit    is integer range 1 to 10;
    constant    high   :integer:=10;
    variable    zero   :integer;
    variable    itl   :mine;
  begin
    zero := 0;
    if i <= high then
      itl:= vero;
      res:= TRUE;
    else 
      res:= FALSE;
    end if;
  end greater;
END c02s02b00x00p02n01i02955ent;

ARCHITECTURE c02s02b00x00p02n01i02955arch OF c02s02b00x00p02n01i02955ent IS
  subtype digit is integer range 1 to 10;
BEGIN
  TESTING: PROCESS
    variable i:digit;
    variable k:boolean;
  BEGIN
    i:= 5;
    greater (i,k);
    wait for 5 ns;
    assert NOT( k=TRUE )
      report "***PASSED TEST: c02s02b00x00p02n01i02955"
      severity NOTE;
    assert ( k=TRUE )
      report "***FAILED TEST: c02s02b00x00p02n01i02955 - Subprogram body syntax test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s02b00x00p02n01i02955arch;
