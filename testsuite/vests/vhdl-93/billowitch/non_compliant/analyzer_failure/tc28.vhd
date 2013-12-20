
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
-- $Id: tc28.vhd,v 1.2 2001-10-26 16:30:22 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s02b00x00p11n01i00028ent IS
END c04s02b00x00p11n01i00028ent;

ARCHITECTURE c04s02b00x00p11n01i00028arch OF c04s02b00x00p11n01i00028ent IS
  type MVL is ('0', '1', 'Z') ;
  type MVL_VEC is array (positive range <>) of MVL;
  function tristate (X:MVL_VEC) return MVL is
  begin
    return '1';
  end tristate ;
  type    T1  is access MVL ;
  subtype ST1 is tristate T1;  -- Failure_here
BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***FAILED TEST: c04s02b00x00p11n01i00028- Subtype indication denoting an access type can not contain a resolution function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s02b00x00p11n01i00028arch;
