
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
-- $Id: tc161.vhd,v 1.2 2001-10-26 16:30:11 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p19n04i00161ent IS
  PORT (   SIGNAL a    : IN bit;
           SIGNAL b    : IN integer;
           SIGNAL c    : IN boolean;
           SIGNAL d    : IN time;
           SIGNAL e,f    : IN real;
           SIGNAL oint    : INOUT integer);
END c04s03b02x02p19n04i00161ent;

ARCHITECTURE c04s03b02x02p19n04i00161arch OF c04s03b02x02p19n04i00161ent IS
  function funct1(   fpar1   :bit   :='1';
                     fpar2   :integer:=455;
                     fpar3   :boolean:=true;
                     fpar4   :time   :=55.77 ns;
                     fpar5   :real   :=34.558) return integer is
  begin
    return 1;
  end funct1;
BEGIN
  TESTING: PROCESS
  BEGIN
    wait for 1 ns;
    oint <= funct1(fpar5=>f,fpar3=>c,fpar2=>b,fpar1=>a,fpar4=>d,fpar5=>e);
    assert FALSE
      report "***FAILED TEST: c04s03b02x02p19n04i00161 - Same formal parameter name can not be used more than once."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p19n04i00161arch;
