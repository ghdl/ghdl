
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
-- $Id: tc986.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p06n01i00986ent IS
END c06s03b00x00p06n01i00986ent;

ARCHITECTURE c06s03b00x00p06n01i00986arch OF c06s03b00x00p06n01i00986ent IS

BEGIN
  TESTING: PROCESS
    type T1 is record
                 S1 : BIT ;
                 S2 : Integer;
               end record;
    type T2 is access T1;
    variable V1 : T2 := new T1'('0',0) ;
    variable V2 : T1;
  BEGIN
    V2 := V1.all ; -- No_Failure_here
    wait for 10 ns;
    assert NOT(V2.S1='0' and V2.S2=0) 
      report "***PASSED TEST: c06s03b00x00p06n01i00986" 
      severity NOTE;
    assert (V2.S1='0' and V2.S2=0) 
      report "***FAILED TEST: c06s03b00x00p06n01i00986 - Prefix of a selected name used to denote an object designated by an access value should be an access type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p06n01i00986arch;
