
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
-- $Id: tc2012.vhd,v 1.2 2001-10-26 16:29:45 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c07s02b02x00p10n01i02012ent IS
END c07s02b02x00p10n01i02012ent;

ARCHITECTURE c07s02b02x00p10n01i02012arch OF c07s02b02x00p10n01i02012ent IS
  SUBTYPE st_ind1 IS INTEGER   RANGE   1   TO     8;     -- index from 1 (POSITIVE)
  SUBTYPE st_ind3 IS CHARACTER RANGE 'a'   TO   'd';     -- non-INTEGER index

  SUBTYPE st_scl1 IS CHARACTER                              ;
  SUBTYPE st_scl3 IS INTEGER   RANGE   1   TO   INTEGER'HIGH;
  
  TYPE t_usa1_1 IS ARRAY (st_ind1 RANGE <>) OF st_scl1;
  TYPE t_usa1_3 IS ARRAY (st_ind3 RANGE <>) OF st_scl3;

  SUBTYPE t_csa1_1 IS t_usa1_1 (st_ind1 );
  SUBTYPE t_csa1_3 IS t_usa1_3 (st_ind3 );

  CONSTANT C0_scl1 : st_scl1 := st_scl1'LEFT ;
  CONSTANT C2_scl1 : st_scl1 := 'Z' ;
  CONSTANT C0_scl3 : st_scl3 := st_scl3'LEFT ;
  CONSTANT C2_scl3 : st_scl3 :=   8 ;

  CONSTANT C0_csa1_1 : t_csa1_1 := ( OTHERS=>C0_scl1);
  CONSTANT C2_csa1_1 : t_csa1_1 := ( t_csa1_1'LEFT|t_csa1_1'RIGHT=>C2_scl1,
                                     OTHERS                      =>C0_scl1);
  CONSTANT C0_csa1_3 : t_csa1_3 := ( OTHERS=>C0_scl3);
  CONSTANT C2_csa1_3 : t_csa1_3 := ( t_csa1_3'LEFT|t_csa1_3'RIGHT=>C2_scl3,
                                     OTHERS                      =>C0_scl3);

BEGIN
  TESTING: PROCESS
--
--          Constant declarations - for unconstrained types
--           other composite type declarations are in package "COMPOSITE"
--
    CONSTANT C0_usa1_1 : t_usa1_1 (st_ind1 ) := C0_csa1_1;
    CONSTANT C0_usa1_3 : t_usa1_3 (st_ind3 ) := C0_csa1_3;

    CONSTANT C2_usa1_1 : t_usa1_1 (st_ind1 ) := C2_csa1_1;
    CONSTANT C2_usa1_3 : t_usa1_3 (st_ind3 ) := C2_csa1_3;
--
--          Composite VARIABLE declarations
--
    VARIABLE V0_usa1_1 : t_usa1_1 (st_ind1 ) ;
    VARIABLE V0_usa1_3 : t_usa1_3 (st_ind3 ) ;
    VARIABLE V0_csa1_1 : t_csa1_1 ;
    VARIABLE V0_csa1_3 : t_csa1_3 ;

    VARIABLE V2_usa1_1 : t_usa1_1 (st_ind1 ) := C2_csa1_1;
    VARIABLE V2_usa1_3 : t_usa1_3 (st_ind3 ) := C2_csa1_3;
    VARIABLE V2_csa1_1 : t_csa1_1 := C2_csa1_1;
    VARIABLE V2_csa1_3 : t_csa1_3 := C2_csa1_3;
--
--          Arrays of the same type, element values, different length
--
    VARIABLE V3_usa1_1 : t_usa1_1 ( 1  TO  7  ) ;
    VARIABLE V3_usa1_3 : t_usa1_3 ('a' TO 'c' ) ;
--
    CONSTANT msg1 : STRING := "ERROR: less than operator failure: ";
    CONSTANT msg2 : STRING := "ERROR: less than or equal operator failure: ";
  BEGIN
--
--          Check less than operator - CONSTANTS (from package 'composite')
    --
    ASSERT C0_usa1_1 < C2_usa1_1 REPORT msg1 & "C0<C2_usa1_1" SEVERITY FAILURE;
    ASSERT C0_usa1_3 < C2_usa1_3 REPORT msg1 & "C0<C2_usa1_3" SEVERITY FAILURE;
    ASSERT C0_csa1_1 < C2_csa1_1 REPORT msg1 & "C0<C2_csa1_1" SEVERITY FAILURE;
    ASSERT C0_csa1_3 < C2_csa1_3 REPORT msg1 & "C0<C2_csa1_3" SEVERITY FAILURE;
--
--          Check less than operator - VARIABLES
--
    ASSERT V0_usa1_1 < V2_usa1_1 REPORT msg1 & "V0<V2_usa1_1" SEVERITY FAILURE;
    ASSERT V0_usa1_3 < V2_usa1_3 REPORT msg1 & "V0<V2_usa1_3" SEVERITY FAILURE;
    ASSERT V0_csa1_1 < V2_csa1_1 REPORT msg1 & "V0<V2_csa1_1" SEVERITY FAILURE;
    ASSERT V0_csa1_3 < V2_csa1_3 REPORT msg1 & "V0<V2_csa1_3" SEVERITY FAILURE;
--
--          Check less than operator - VARIABLES and CONSTANTS
--
    ASSERT V0_usa1_1 < C2_usa1_1 REPORT msg1 & "V0<C2_usa1_1" SEVERITY FAILURE;
    ASSERT V0_usa1_3 < C2_usa1_3 REPORT msg1 & "V0<C2_usa1_3" SEVERITY FAILURE;
    ASSERT V0_csa1_1 < C2_csa1_1 REPORT msg1 & "V0<C2_csa1_1" SEVERITY FAILURE;
    ASSERT V0_csa1_3 < C2_csa1_3 REPORT msg1 & "V0<C2_csa1_3" SEVERITY FAILURE;
--
--          Check less than operator - same type, element values : diff array length
--
    ASSERT V3_usa1_1 < V2_usa1_1 REPORT msg1 & "V3<V2_usa1_1" SEVERITY FAILURE;
    ASSERT V3_usa1_3 < V2_usa1_3 REPORT msg1 & "V3<V2_usa1_3" SEVERITY FAILURE;
--
--          Check less than or equal operator - CONSTANTS (from package 'composite')
--
    ASSERT C0_usa1_1 <= C2_usa1_1 REPORT msg2 & "C0<=C2_usa1_1" SEVERITY FAILURE;
    ASSERT C0_usa1_3 <= C2_usa1_3 REPORT msg2 & "C0<=C2_usa1_3" SEVERITY FAILURE;
    ASSERT C0_csa1_1 <= C2_csa1_1 REPORT msg2 & "C0<=C2_csa1_1" SEVERITY FAILURE;
    ASSERT C0_csa1_3 <= C2_csa1_3 REPORT msg2 & "C0<=C2_csa1_3" SEVERITY FAILURE;
--
--          Check less than or equal operator - VARIABLES
--
    ASSERT V0_usa1_1 <= V2_usa1_1 REPORT msg2 & "V0<=V2_usa1_1" SEVERITY FAILURE;
    ASSERT V0_usa1_3 <= V2_usa1_3 REPORT msg2 & "V0<=V2_usa1_3" SEVERITY FAILURE;
    ASSERT V0_csa1_1 <= V2_csa1_1 REPORT msg2 & "V0<=V2_csa1_1" SEVERITY FAILURE;
    ASSERT V0_csa1_3 <= V2_csa1_3 REPORT msg2 & "V0<=V2_csa1_3" SEVERITY FAILURE;
--
--          Check less than or equal operator - VARIABLES and CONSTANTS
--
    ASSERT V0_usa1_1 <= C2_usa1_1 REPORT msg2 & "V0<=C2_usa1_1" SEVERITY FAILURE;
    ASSERT V0_usa1_3 <= C2_usa1_3 REPORT msg2 & "V0<=C2_usa1_3" SEVERITY FAILURE;
    ASSERT V0_csa1_1 <= C2_csa1_1 REPORT msg2 & "V0<=C2_csa1_1" SEVERITY FAILURE;
    ASSERT V0_csa1_3 <= C2_csa1_3 REPORT msg2 & "V0<=C2_csa1_3" SEVERITY FAILURE;
--
--          Check less than or equal operator - same type, element values : diff array length
--
    ASSERT V3_usa1_1 <= V2_usa1_1 REPORT msg2 & "V3<=V2_usa1_1" SEVERITY FAILURE;
    ASSERT V3_usa1_3 <= V2_usa1_3 REPORT msg2 & "V3<=V2_usa1_3" SEVERITY FAILURE;
--
--          Check less than or equal operator - CONSTANTS (from package 'composite')
--
    ASSERT C2_usa1_1 <= C2_usa1_1 REPORT msg2 & "C2<=C2_usa1_1" SEVERITY FAILURE;
    ASSERT C2_usa1_3 <= C2_usa1_3 REPORT msg2 & "C2<=C2_usa1_3" SEVERITY FAILURE;
    ASSERT C2_csa1_1 <= C2_csa1_1 REPORT msg2 & "C2<=C2_csa1_1" SEVERITY FAILURE;
    ASSERT C2_csa1_3 <= C2_csa1_3 REPORT msg2 & "C2<=C2_csa1_3" SEVERITY FAILURE;
--
--          Check less than or equal operator - VARIABLES
--
    ASSERT V2_usa1_1 <= V2_usa1_1 REPORT msg2 & "V2<=V2_usa1_1" SEVERITY FAILURE;
    ASSERT V2_usa1_3 <= V2_usa1_3 REPORT msg2 & "V2<=V2_usa1_3" SEVERITY FAILURE;
    ASSERT V2_csa1_1 <= V2_csa1_1 REPORT msg2 & "V2<=V2_csa1_1" SEVERITY FAILURE;
    ASSERT V2_csa1_3 <= V2_csa1_3 REPORT msg2 & "V2<=V2_csa1_3" SEVERITY FAILURE;
--
--          Check less than or equal operator - VARIABLES and CONSTANTS
--
    ASSERT V2_usa1_1 <= C2_usa1_1 REPORT msg2 & "V2<=C2_usa1_1" SEVERITY FAILURE;
    ASSERT V2_usa1_3 <= C2_usa1_3 REPORT msg2 & "V2<=C2_usa1_3" SEVERITY FAILURE;
    ASSERT V2_csa1_1 <= C2_csa1_1 REPORT msg2 & "V2<=C2_csa1_1" SEVERITY FAILURE;
    ASSERT V2_csa1_3 <= C2_csa1_3 REPORT msg2 & "V2<=C2_csa1_3" SEVERITY FAILURE;
    assert NOT(   C0_usa1_1 < C2_usa1_1   and
                  C0_usa1_3 < C2_usa1_3   and
                  C0_csa1_1 < C2_csa1_1   and
                  C0_csa1_3 < C2_csa1_3   and
                  V0_usa1_1 < V2_usa1_1   and
                  V0_usa1_3 < V2_usa1_3   and
                  V0_csa1_1 < V2_csa1_1   and
                  V0_csa1_3 < V2_csa1_3   and
                  V0_usa1_1 < C2_usa1_1   and
                  V0_usa1_3 < C2_usa1_3   and
                  V0_csa1_1 < C2_csa1_1   and
                  V0_csa1_3 < C2_csa1_3   and
                  V3_usa1_1 < V2_usa1_1   and
                  V3_usa1_3 < V2_usa1_3   and
                  C0_usa1_1 <= C2_usa1_1   and
                  C0_usa1_3 <= C2_usa1_3   and
                  C0_csa1_1 <= C2_csa1_1   and
                  C0_csa1_3 <= C2_csa1_3   and
                  V0_usa1_1 <= V2_usa1_1   and
                  V0_usa1_3 <= V2_usa1_3   and
                  V0_csa1_1 <= V2_csa1_1   and
                  V0_csa1_3 <= V2_csa1_3   and
                  V0_usa1_1 <= C2_usa1_1   and
                  V0_usa1_3 <= C2_usa1_3   and
                  V0_csa1_1 <= C2_csa1_1   and
                  V0_csa1_3 <= C2_csa1_3   and
                  V3_usa1_1 <= V2_usa1_1   and
                  V3_usa1_3 <= V2_usa1_3   and
                  C2_usa1_1 <= C2_usa1_1   and
                  C2_usa1_3 <= C2_usa1_3   and
                  C2_csa1_1 <= C2_csa1_1   and
                  C2_csa1_3 <= C2_csa1_3   and
                  V2_usa1_1 <= V2_usa1_1   and
                  V2_usa1_3 <= V2_usa1_3   and
                  V2_csa1_1 <= V2_csa1_1   and
                  V2_csa1_3 <= V2_csa1_3   and
                  V2_usa1_1 <= C2_usa1_1   and
                  V2_usa1_3 <= C2_usa1_3   and
                  V2_csa1_1 <= C2_csa1_1   and
                  V2_csa1_3 <= C2_csa1_3   )
      report "***PASSED TEST: c07s02b02x00p10n01i02012" 
      severity NOTE;
    assert (   C0_usa1_1 < C2_usa1_1   and
               C0_usa1_3 < C2_usa1_3   and
               C0_csa1_1 < C2_csa1_1   and
               C0_csa1_3 < C2_csa1_3   and
               V0_usa1_1 < V2_usa1_1   and
               V0_usa1_3 < V2_usa1_3   and
               V0_csa1_1 < V2_csa1_1   and
               V0_csa1_3 < V2_csa1_3   and
               V0_usa1_1 < C2_usa1_1   and
               V0_usa1_3 < C2_usa1_3   and
               V0_csa1_1 < C2_csa1_1   and
               V0_csa1_3 < C2_csa1_3   and
               V3_usa1_1 < V2_usa1_1   and
               V3_usa1_3 < V2_usa1_3   and
               C0_usa1_1 <= C2_usa1_1   and
               C0_usa1_3 <= C2_usa1_3   and
               C0_csa1_1 <= C2_csa1_1   and
               C0_csa1_3 <= C2_csa1_3   and
               V0_usa1_1 <= V2_usa1_1   and
               V0_usa1_3 <= V2_usa1_3   and
               V0_csa1_1 <= V2_csa1_1   and
               V0_csa1_3 <= V2_csa1_3   and
               V0_usa1_1 <= C2_usa1_1   and
               V0_usa1_3 <= C2_usa1_3   and
               V0_csa1_1 <= C2_csa1_1   and
               V0_csa1_3 <= C2_csa1_3   and
               V3_usa1_1 <= V2_usa1_1   and
               V3_usa1_3 <= V2_usa1_3   and
               C2_usa1_1 <= C2_usa1_1   and
               C2_usa1_3 <= C2_usa1_3   and
               C2_csa1_1 <= C2_csa1_1   and
               C2_csa1_3 <= C2_csa1_3   and
               V2_usa1_1 <= V2_usa1_1   and
               V2_usa1_3 <= V2_usa1_3   and
               V2_csa1_1 <= V2_csa1_1   and
               V2_csa1_3 <= V2_csa1_3   and
               V2_usa1_1 <= C2_usa1_1   and
               V2_usa1_3 <= C2_usa1_3   and
               V2_csa1_1 <= C2_csa1_1   and
               V2_csa1_3 <= C2_csa1_3   )
      report "***FAILED TEST: c07s02b02x00p10n01i02012 - Ordering operators <, <= for composite type test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c07s02b02x00p10n01i02012arch;
