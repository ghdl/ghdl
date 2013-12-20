
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
-- $Id: tc925.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c10s03b00x00p22n01i00925ent IS
END c10s03b00x00p22n01i00925ent;

ARCHITECTURE c10s03b00x00p22n01i00925arch OF c10s03b00x00p22n01i00925ent IS
  constant x : integer := 3;
  procedure xxx is
    constant x : integer := 5;
    variable y : bit;
  begin
    if x > 3 then 
      y := '1';
    else 
      y := '0';
    end if;
    assert NOT( y='1' )
      report "***PASSED TEST: c10s03b00x00p22n01i00925"
      severity NOTE;
    assert ( y='1' )
      report "***FAILED TEST: c10s03b00x00p22n01i00925 - Within the specification of a subprogram, every declaration with the same designator as the sybprogram is hidden."
      severity ERROR;
  end xxx;
BEGIN
  xxx;

END c10s03b00x00p22n01i00925arch;
