
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
-- $Id: tc2883.vhd,v 1.2 2001-10-26 16:29:50 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c02s01b01x00p03n01i02883ent IS
  procedure howe (k:in real; v:out real) is
  begin
    v := k;
  end howe;
END c02s01b01x00p03n01i02883ent;

ARCHITECTURE c02s01b01x00p03n01i02883arch OF c02s01b01x00p03n01i02883ent IS
  constant k:real:=3.1415;
BEGIN
  TESTING: PROCESS
    variable v:real;
  BEGIN
    howe(k,v);
    wait for 5 ns;
    assert NOT( k=v )
      report "***PASSED TEST: c02s01b01x00p03n01i02883"
      severity NOTE;
    assert ( k=v )
      report "***FAILED TEST: c02s01b01x00p03n01i02883 - Wrong value returned from procedure."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c02s01b01x00p03n01i02883arch;
