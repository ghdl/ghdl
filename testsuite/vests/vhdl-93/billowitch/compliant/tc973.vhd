
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
-- $Id: tc973.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c06s03b00x00p05n01i00973ent IS
END c06s03b00x00p05n01i00973ent;

ARCHITECTURE c06s03b00x00p05n01i00973arch OF c06s03b00x00p05n01i00973ent IS
  TYPE simple_record_2_type IS
    RECORD
      a2 : integer;
      b2 : integer;
    END RECORD;

  TYPE array_of_records_type IS
    ARRAY (20 TO 30) OF simple_record_2_type;

  SIGNAL sr : array_of_records_type;
BEGIN
  TESTING: PROCESS
    VARIABLE ar,br : array_of_records_type;

    FUNCTION convert (ain : array_of_records_type) RETURN integer IS
    BEGIN
      RETURN (ain(25).b2);
    END;
  BEGIN
    wait for 1 ns;
    br(20).b2 := 8;
    ar(30).b2 := br(20).b2;

    ar(30).b2 := 8;
    ar(20).a2 := ar(30).b2;

    sr(30).b2 <= 8;
    wait for 1 ns;
    sr(20).a2 <= sr(30).b2;
    wait for 1 ns;

    ar(25).b2 := 3;
    sr(25).b2 <= 3;
    wait for 1 ns;
    
    assert NOT((ar(30).b2 = 8)   AND
               (ar(20).a2 = 8)   AND
               (sr(20).a2 = 8)   AND
               (convert(ar) = 3) AND
               (convert(sr) = 3)) 
      report "***PASSED TEST: c06s03b00x00p05n01i00973" 
      severity NOTE;
    assert    ((ar(30).b2 = 8)   AND
               (ar(20).a2 = 8)   AND
               (sr(20).a2 = 8)   AND
               (convert(ar) = 3) AND
               (convert(sr) = 3)) 
      report "***FAILED TEST: c06s03b00x00p05n01i00973 - The prefix fo the selected names can be an array." 
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p05n01i00973arch;
