
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
-- $Id: tc2679.vhd,v 1.2 2001-10-26 16:29:49 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c13s03b01x00p05n02i02679pkg is
  fUnction F rEturn BooLEAN;
end c13s03b01x00p05n02i02679pkg ;

package body c13s03b01x00p05n02i02679pkg is
  fUnction F rEturn BooLEAN is
    tYpe TyP_1 is ranGe 1 to 10;
    suBtyPe STYp_1 is TYP_1 range 1 to 5;
    type ReC_1 is rEcorD
                    RV_1: BOOlean;
                    RV_2: intEGER;
                    RV_3: REal;
                  end RECord;
    VariabLe V1: STYP_1;
  beGin
    v1 := 4;
    rETurn FalSe;
  enD f;
end c13s03b01x00p05n02i02679pkg ;

ENTITY c13s03b01x00p05n02i02679ent IS
END c13s03b01x00p05n02i02679ent;

ARCHITECTURE c13s03b01x00p05n02i02679arch OF c13s03b01x00p05n02i02679ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert FALSE 
      report "***PASSED TEST: c13s03b01x00p05n02i02679"
      severity NOTE;
    wait;
  END PROCESS TESTING;

END c13s03b01x00p05n02i02679arch;
