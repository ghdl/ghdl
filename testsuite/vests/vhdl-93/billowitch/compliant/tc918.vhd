
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
-- $Id: tc918.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s03b00x00p13n01i00918ent IS
  procedure subprogram ( a : integer; b : real ) is
  begin
    assert ( b = real (a) ) report "not the same" severity FAILURE;
    assert NOT( b = real(a) )
      report "***PASSED TEST: c10s03b00x00p13n01i00918"
      severity NOTE;
    assert ( b = real(a) )
      report "***FAILED TEST: c10s03b00x00p13n01i00918 - "
      severity ERROR;
  end subprogram;
END c10s03b00x00p13n01i00918ent;

ARCHITECTURE c10s03b00x00p13n01i00918arch OF c10s03b00x00p13n01i00918ent IS

BEGIN
  subprogram ( a => 10  , b => 10.0 );

END c10s03b00x00p13n01i00918arch;
