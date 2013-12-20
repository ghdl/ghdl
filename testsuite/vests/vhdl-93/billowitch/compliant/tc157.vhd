
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
-- $Id: tc157.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c04s03b02x02p18n01i00157ent IS
END c04s03b02x02p18n01i00157ent;

ARCHITECTURE c04s03b02x02p18n01i00157arch OF c04s03b02x02p18n01i00157ent IS
  TYPE TwoBy3By4Type IS ARRAY (1 TO 2,1 TO 3,1 TO 4) OF integer RANGE 111 TO 234;

  FUNCTION func1(fp1:TwoBy3By4Type:=
                 (
                   (  (111,112,113,114),
                      (121,122,123,124),
                      (131,132,133,134) ),

                   (  (211,212,213,214),
                      (221,222,223,224),
                      (231,232,233,234) )
                   )) RETURN BOOLEAN;

  FUNCTION func1(fp1:TwoBy3By4Type:=
                 (
                   (  (111,112,113,114),
                      (121,122,123,124),
                      (131,132,133,134) ),

                   (  (211,212,213,214),
                      (221,222,223,224),
                      (231,232,233,234) )
                   )) RETURN BOOLEAN IS
    VARIABLE fv1 : TwoBy3By4Type :=
      (
        (  (111,112,113,114),
           (121,122,123,124),
           (131,132,133,134) ),

        (  (211,212,213,214),
           (221,222,223,224),
           (231,232,233,234) )
        );
  BEGIN
    RETURN ((fv1 = fp1) AND (fp1(2,2,3) = 223));
  END;

BEGIN
  TESTING: PROCESS
    
    VARIABLE v1,v2 : TwoBy3By4Type :=
      (
        (  (111,112,113,114),
           (121,122,123,124),
           (131,132,133,134) ),
        
        (  (211,212,213,214),
           (221,222,223,224),
           (231,232,233,234) )
        );
    
  BEGIN
    wait for 5 ns;
    assert NOT( func1(v1))
      report "***PASSED TEST: c04s03b02x02p18n01i00157"
      severity NOTE;
    assert ( func1(v1))
      report "***FAILED TEST: c04s03b02x02p18n01i00157 - Multi-dimensional array test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c04s03b02x02p18n01i00157arch;
