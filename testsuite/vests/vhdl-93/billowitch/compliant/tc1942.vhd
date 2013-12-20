
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
-- $Id: tc1942.vhd,v 1.2 2001-10-26 16:29:44 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b01x00p01n02i01942ent IS
END c07s02b01x00p01n02i01942ent;

ARCHITECTURE c07s02b01x00p01n02i01942arch OF c07s02b01x00p01n02i01942ent IS
  TYPE b4 IS ARRAY (1 TO 4) OF BOOLEAN;

  CONSTANT T : BOOLEAN := TRUE;
  CONSTANT F : BOOLEAN :=FALSE;
BEGIN
  TESTING: PROCESS
  BEGIN
--
--          Test operators on one-dimesioned arrays of BOOLEAN
--
    ASSERT ( b4'( T,T,F,F ) AND  b4'( T,F,T,F ) ) = b4'( T,F,F,F ) 
      REPORT "ERROR: composite AND operator failed : BOOLEAN"
      SEVERITY FAILURE;
    ASSERT ( b4'( T,T,F,F )  OR  b4'( T,F,T,F ) ) = b4'( T,T,T,F ) 
      REPORT "ERROR: composite  OR operator failed : BOOLEAN"
      SEVERITY FAILURE;
    ASSERT ( b4'( T,T,F,F ) NAND b4'( T,F,T,F ) ) = b4'( F,T,T,T ) 
      REPORT "ERROR: composite NAND operator failed : BOOLEAN"
      SEVERITY FAILURE;
    ASSERT ( b4'( T,T,F,F ) NOR  b4'( T,F,T,F ) ) = b4'( F,F,F,T ) 
      REPORT "ERROR: composite NOR operator failed : BOOLEAN"
      SEVERITY FAILURE;
    ASSERT ( b4'( T,T,F,F ) XOR  b4'( T,F,T,F ) ) = b4'( F,T,T,F ) 
      REPORT "ERROR: composite XOR operator failed : BOOLEAN"
      SEVERITY FAILURE;
    ASSERT (                NOT  b4'( T,T,F,F ) ) = b4'( F,F,T,T ) 
      REPORT "ERROR: composite NOT operator failed : BOOLEAN"
      SEVERITY FAILURE;
    wait for 5 ns;
    assert NOT(   (( b4'( T,T,F,F ) AND  b4'( T,F,T,F ) ) = b4'( T,F,F,F ))   and
                  (( b4'( T,T,F,F )  OR  b4'( T,F,T,F ) ) = b4'( T,T,T,F ))   and
                  (( b4'( T,T,F,F ) NAND b4'( T,F,T,F ) ) = b4'( F,T,T,T ))   and
                  (( b4'( T,T,F,F ) NOR  b4'( T,F,T,F ) ) = b4'( F,F,F,T ))   and
                  (( b4'( T,T,F,F ) XOR  b4'( T,F,T,F ) ) = b4'( F,T,T,F ))   and
                  ((                NOT  b4'( T,T,F,F ) ) = b4'( F,F,T,T ))   )
      report "***PASSED TEST: c07s02b01x00p01n02i01942" 
      severity NOTE;
    assert (   (( b4'( T,T,F,F ) AND  b4'( T,F,T,F ) ) = b4'( T,F,F,F ))   and
               (( b4'( T,T,F,F )  OR  b4'( T,F,T,F ) ) = b4'( T,T,T,F ))   and
               (( b4'( T,T,F,F ) NAND b4'( T,F,T,F ) ) = b4'( F,T,T,T ))   and
               (( b4'( T,T,F,F ) NOR  b4'( T,F,T,F ) ) = b4'( F,F,F,T ))   and
               (( b4'( T,T,F,F ) XOR  b4'( T,F,T,F ) ) = b4'( F,T,T,F ))   and
               ((                NOT  b4'( T,T,F,F ) ) = b4'( F,F,T,T ))   )
      report "***FAILED TEST: c07s02b01x00p01n02i01942 - Logical operators should be valid for any one-dimensional array type whose element type is BOOLEAN."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b01x00p01n02i01942arch;
