
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
-- $Id: tc955.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s01b00x00p10n02i00955ent IS
END c06s01b00x00p10n02i00955ent;

ARCHITECTURE c06s01b00x00p10n02i00955arch OF c06s01b00x00p10n02i00955ent IS

  signal PT : boolean;
  subtype ONE is integer range 1 to 1;
  type R1 is record
               X1: ONE;
               RE1: BOOLEAN;
             end record;
  function rr1(i : integer) return R1 is
    variable vr : r1;
  begin
    return vr;
  end rr1;
  attribute AT1       : R1;
  attribute AT1 of PT : signal is rr1(3);
  type A1 is array (BOOLEAN) of BOOLEAN;
BEGIN
  TESTING: PROCESS
    variable V1: BOOLEAN;
  BEGIN
    V1 := PT'AT1.RE1;
    assert NOT( V1=FALSE )
      report "***PASSED TEST: c06s01b00x00p10n02i00955"
      severity NOTE;
    assert ( V1=FALSE )
      report "***FAILED TEST: c06s01b00x00p10n02i00955 - The prefix of a name is a function call."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s01b00x00p10n02i00955arch;
