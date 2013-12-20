
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
-- $Id: tc521.vhd,v 1.2 2001-10-26 16:29:56 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

PACKAGE c03s03b00x00p03n04i00521pkg IS
--
--          Index types for array declarations
--
  SUBTYPE st_ind1 IS INTEGER   RANGE   1   TO     8;     -- index from 1 (POSITIVE)
  SUBTYPE st_ind2 IS INTEGER   RANGE   0   TO     3;     -- index from 0 (NATURAL)        
  SUBTYPE st_ind3 IS CHARACTER RANGE 'a'   TO   'd';     -- non-INTEGER index
  SUBTYPE st_ind4 IS INTEGER   RANGE   0 DOWNTO  -3;     -- descending range
--
--          Scalar type for subelements
--
  SUBTYPE st_scl1 IS CHARACTER                              ;
  SUBTYPE st_scl3 IS INTEGER   RANGE   1   TO   INTEGER'HIGH;
  SUBTYPE st_scl4 IS REAL      RANGE 0.0   TO         1024.0;
  
-- -----------------------------------------------------------------------------------------
--      Composite type declarations
-- -----------------------------------------------------------------------------------------
--
--          Records of scalars
--
  TYPE t_scre_1 IS RECORD
                     left   :  st_scl1;
                     second :  TIME;
                     third  :  st_scl3;
                     right  :  st_scl4;
                   END RECORD;
--
--          Unconstrained arrays of scalars
--
  TYPE t_usa1_1 IS ARRAY (st_ind1 RANGE <>) OF st_scl1;
  TYPE t_usa1_2 IS ARRAY (st_ind2 RANGE <>) OF TIME;
  TYPE t_usa1_3 IS ARRAY (st_ind3 RANGE <>) OF st_scl3;
  TYPE t_usa1_4 IS ARRAY (st_ind4 RANGE <>) OF st_scl4;
  
  TYPE t_usa2_1 IS ARRAY (st_ind2 RANGE <>,
                          st_ind1 RANGE <>) OF st_scl1;
  TYPE t_usa3_1 IS ARRAY (st_ind3 RANGE <>,
                          st_ind2 RANGE <>,
                          st_ind1 RANGE <>) OF st_scl1;
  TYPE t_usa4_1 IS ARRAY (st_ind4 RANGE <>,
                          st_ind3 RANGE <>,
                          st_ind2 RANGE <>,
                          st_ind1 RANGE <>) OF st_scl1;
--
--
--          Constrained arrays of scalars (make compatable with unconstrained types
--
  SUBTYPE t_csa1_1 IS t_usa1_1 (st_ind1 );
  SUBTYPE t_csa1_2 IS t_usa1_2 (st_ind2 );
  SUBTYPE t_csa1_3 IS t_usa1_3 (st_ind3 );
  SUBTYPE t_csa1_4 IS t_usa1_4 (st_ind4 );
  
  SUBTYPE t_csa2_1 IS t_usa2_1 (st_ind2 ,                         -- ( i2, i1 ) of CHAR
                                st_ind1 );
  SUBTYPE t_csa3_1 IS t_usa3_1 (st_ind3 ,                         -- ( i3, i2, i1) of CHAR
                                st_ind2 ,
                                st_ind1 );
  SUBTYPE t_csa4_1 IS t_usa4_1 (st_ind4 ,                         -- ( i4, i3, i2, i1 ) of CHAR
                                st_ind3 ,
                                st_ind2 ,
                                st_ind1 );
--
--
--          constrained arrays of composites
--
  TYPE t_cca1_1 IS ARRAY (st_ind1) OF t_scre_1;          -- ( i1 ) is RECORD of scalar
  TYPE t_cca1_2 IS ARRAY (st_ind2) OF t_csa1_1;          --             ( i2 )( i1 ) is CHAR
  TYPE t_cca1_3 IS ARRAY (st_ind3) OF t_cca1_2;          --       ( i3 )( i2 )( i1 ) is CHAR
  TYPE t_cca1_4 IS ARRAY (st_ind4) OF t_cca1_3;          -- ( i4 )( i3 )( i2 )( i1 ) is CHAR
  
  TYPE t_cca2_1 IS ARRAY (st_ind3) OF t_csa2_1;          --       ( i3 )( i2,   i1 ) is CHAR
  TYPE t_cca2_2 IS ARRAY (st_ind4,                       -- ( i4,   i3 )( i2,   i1 ) of CHAR
                          st_ind3) OF t_csa2_1;
  TYPE t_cca3_1 IS ARRAY (st_ind4,                       -- ( i4,   i3,   i2 )( i1 ) of CHAR
                          st_ind3,
                          st_ind2) OF t_csa1_1;
  TYPE t_cca3_2 IS ARRAY (st_ind4) OF t_csa3_1;          -- ( i4 )( i3,   i2,   i1 ) is CHAR
--
--          Records of composites
--
  TYPE t_cmre_1 IS RECORD
                     left   :  t_csa1_1;                                   -- .fN(i1) is CHAR
                     second :  t_scre_1;                                   -- .fN.fN
                   END RECORD;

  TYPE t_cmre_2 IS RECORD
                     left   ,
                       second ,
                       third  ,
                       right  :  t_csa1_1;                               --        .fN(i1) is CHAR        
                   END RECORD;
--
--          Mixed Records/arrays
--
  TYPE t_cca1_7 IS ARRAY (st_ind3) OF t_cmre_2;          --    (i3).fN(i1) is CHAR        
  TYPE t_cmre_3 IS RECORD
                     left   ,
                       second ,
                       third  ,
                       right  :  t_cca1_7;                               -- .fN(i3).fN(i1) is CHAR        
                   END RECORD;
  
--
--          TYPE declarations for resolution function (Constrained types only)
--
  TYPE t_scre_1_vct IS ARRAY (POSITIVE RANGE <>) OF t_scre_1;
  TYPE t_csa1_1_vct IS ARRAY (POSITIVE RANGE <>) OF t_csa1_1;
  TYPE t_csa1_2_vct IS ARRAY (POSITIVE RANGE <>) OF t_csa1_2;
  TYPE t_csa1_3_vct IS ARRAY (POSITIVE RANGE <>) OF t_csa1_3;
  TYPE t_csa1_4_vct IS ARRAY (POSITIVE RANGE <>) OF t_csa1_4;
  TYPE t_csa2_1_vct IS ARRAY (POSITIVE RANGE <>) OF t_csa2_1;
  TYPE t_csa3_1_vct IS ARRAY (POSITIVE RANGE <>) OF t_csa3_1;
  TYPE t_csa4_1_vct IS ARRAY (POSITIVE RANGE <>) OF t_csa4_1;
  TYPE t_cca1_1_vct IS ARRAY (POSITIVE RANGE <>) OF t_cca1_1;
  TYPE t_cca1_2_vct IS ARRAY (POSITIVE RANGE <>) OF t_cca1_2;
  TYPE t_cca1_3_vct IS ARRAY (POSITIVE RANGE <>) OF t_cca1_3;
  TYPE t_cca1_4_vct IS ARRAY (POSITIVE RANGE <>) OF t_cca1_4;
  TYPE t_cca2_1_vct IS ARRAY (POSITIVE RANGE <>) OF t_cca2_1;
  TYPE t_cca2_2_vct IS ARRAY (POSITIVE RANGE <>) OF t_cca2_2;
  TYPE t_cca3_1_vct IS ARRAY (POSITIVE RANGE <>) OF t_cca3_1;
  TYPE t_cca3_2_vct IS ARRAY (POSITIVE RANGE <>) OF t_cca3_2;
  TYPE t_cmre_1_vct IS ARRAY (POSITIVE RANGE <>) OF t_cmre_1;
  TYPE t_cmre_2_vct IS ARRAY (POSITIVE RANGE <>) OF t_cmre_2;
  TYPE t_cca1_7_vct IS ARRAY (POSITIVE RANGE <>) OF t_cca1_7;
  TYPE t_cmre_3_vct IS ARRAY (POSITIVE RANGE <>) OF t_cmre_3;
--
--          Declaration of Resolution Functions
--
  FUNCTION rf_scre_1 ( v: t_scre_1_vct ) RETURN t_scre_1;
  FUNCTION rf_csa1_1 ( v: t_csa1_1_vct ) RETURN t_csa1_1;
  FUNCTION rf_csa1_2 ( v: t_csa1_2_vct ) RETURN t_csa1_2;
  FUNCTION rf_csa1_3 ( v: t_csa1_3_vct ) RETURN t_csa1_3;
  FUNCTION rf_csa1_4 ( v: t_csa1_4_vct ) RETURN t_csa1_4;
  FUNCTION rf_csa2_1 ( v: t_csa2_1_vct ) RETURN t_csa2_1;
  FUNCTION rf_csa3_1 ( v: t_csa3_1_vct ) RETURN t_csa3_1;
  FUNCTION rf_csa4_1 ( v: t_csa4_1_vct ) RETURN t_csa4_1;
  FUNCTION rf_cca1_1 ( v: t_cca1_1_vct ) RETURN t_cca1_1;
  FUNCTION rf_cca1_2 ( v: t_cca1_2_vct ) RETURN t_cca1_2;
  FUNCTION rf_cca1_3 ( v: t_cca1_3_vct ) RETURN t_cca1_3;
  FUNCTION rf_cca1_4 ( v: t_cca1_4_vct ) RETURN t_cca1_4;
  FUNCTION rf_cca2_1 ( v: t_cca2_1_vct ) RETURN t_cca2_1;
  FUNCTION rf_cca2_2 ( v: t_cca2_2_vct ) RETURN t_cca2_2;
  FUNCTION rf_cca3_1 ( v: t_cca3_1_vct ) RETURN t_cca3_1;
  FUNCTION rf_cca3_2 ( v: t_cca3_2_vct ) RETURN t_cca3_2;
  FUNCTION rf_cmre_1 ( v: t_cmre_1_vct ) RETURN t_cmre_1;
  FUNCTION rf_cmre_2 ( v: t_cmre_2_vct ) RETURN t_cmre_2;
  FUNCTION rf_cca1_7 ( v: t_cca1_7_vct ) RETURN t_cca1_7;
  FUNCTION rf_cmre_3 ( v: t_cmre_3_vct ) RETURN t_cmre_3;
--
--          Resolved SUBTYPE declaration
--
  SUBTYPE rst_scre_1 IS rf_scre_1 t_scre_1 ;
  SUBTYPE rst_csa1_1 IS rf_csa1_1 t_csa1_1 ;
  SUBTYPE rst_csa1_2 IS rf_csa1_2 t_csa1_2 ;
  SUBTYPE rst_csa1_3 IS rf_csa1_3 t_csa1_3 ;
  SUBTYPE rst_csa1_4 IS rf_csa1_4 t_csa1_4 ;
  SUBTYPE rst_csa2_1 IS rf_csa2_1 t_csa2_1 ;
  SUBTYPE rst_csa3_1 IS rf_csa3_1 t_csa3_1 ;
  SUBTYPE rst_csa4_1 IS rf_csa4_1 t_csa4_1 ;
  SUBTYPE rst_cca1_1 IS rf_cca1_1 t_cca1_1 ;
  SUBTYPE rst_cca1_2 IS rf_cca1_2 t_cca1_2 ;
  SUBTYPE rst_cca1_3 IS rf_cca1_3 t_cca1_3 ;
  SUBTYPE rst_cca1_4 IS rf_cca1_4 t_cca1_4 ;
  SUBTYPE rst_cca2_1 IS rf_cca2_1 t_cca2_1 ;
  SUBTYPE rst_cca2_2 IS rf_cca2_2 t_cca2_2 ;
  SUBTYPE rst_cca3_1 IS rf_cca3_1 t_cca3_1 ;
  SUBTYPE rst_cca3_2 IS rf_cca3_2 t_cca3_2 ;
  SUBTYPE rst_cmre_1 IS rf_cmre_1 t_cmre_1 ;
  SUBTYPE rst_cmre_2 IS rf_cmre_2 t_cmre_2 ;
  SUBTYPE rst_cca1_7 IS rf_cca1_7 t_cca1_7 ;
  SUBTYPE rst_cmre_3 IS rf_cmre_3 t_cmre_3 ;
--
--          Functions declarations for multi-dimensional comosite values
--
  FUNCTION F_csa2_1 ( v0,v2 : IN st_scl1 ) RETURN t_csa2_1 ;
  FUNCTION F_csa3_1 ( v0,v2 : IN st_scl1 ) RETURN t_csa3_1 ;
  FUNCTION F_csa4_1 ( v0,v2 : IN st_scl1 ) RETURN t_csa4_1 ;
  FUNCTION F_cca2_2 ( v0,v2 : IN t_csa2_1 ) RETURN t_cca2_2 ;
  FUNCTION F_cca3_1 ( v0,v2 : IN t_csa1_1 ) RETURN t_cca3_1 ;
  
-- -------------------------------------------------------------------------------------------
--      Data values for Composite Types
-- -------------------------------------------------------------------------------------------
  CONSTANT CX_scl1 : st_scl1 := 'X' ;
  CONSTANT C0_scl1 : st_scl1 := st_scl1'LEFT ;
  CONSTANT C1_scl1 : st_scl1 := 'A' ;
  CONSTANT C2_scl1 : st_scl1 := 'Z' ;
  
  CONSTANT CX_scl2 : TIME := 99 fs ;
  CONSTANT C0_scl2 : TIME := TIME'LEFT ;
  CONSTANT C1_scl2 : TIME := 0 fs;
  CONSTANT C2_scl2 : TIME := 2 ns;
  
  CONSTANT CX_scl3 : st_scl3 :=  15 ;
  CONSTANT C0_scl3 : st_scl3 := st_scl3'LEFT ;
  CONSTANT C1_scl3 : st_scl3 :=   6 ;
  CONSTANT C2_scl3 : st_scl3 :=   8 ;
  
  CONSTANT CX_scl4 : st_scl4 := 99.9 ;
  CONSTANT C0_scl4 : st_scl4 := st_scl4'LEFT ;
  CONSTANT C1_scl4 : st_scl4 :=  1.0 ;
  CONSTANT C2_scl4 : st_scl4 :=  2.1 ;
  
  CONSTANT CX_scre_1 : t_scre_1 := ( CX_scl1, CX_scl2, CX_scl3, CX_scl4 );
  CONSTANT C0_scre_1 : t_scre_1 := ( C0_scl1, C0_scl2, C0_scl3, C0_scl4 );
  CONSTANT C1_scre_1 : t_scre_1 := ( C1_scl1, C1_scl2, C1_scl3, C1_scl4 );
  CONSTANT C2_scre_1 : t_scre_1 := ( C2_scl1, C0_scl2, C0_scl3, C2_scl4 );
  
  CONSTANT CX_csa1_1 : t_csa1_1 := ( OTHERS=>CX_scl1);
  CONSTANT C0_csa1_1 : t_csa1_1 := ( OTHERS=>C0_scl1);
  CONSTANT C1_csa1_1 : t_csa1_1 := ( OTHERS=>C1_scl1);
  CONSTANT C2_csa1_1 : t_csa1_1 := ( t_csa1_1'LEFT|t_csa1_1'RIGHT=>C2_scl1,
                                     OTHERS                      =>C0_scl1);
  
  CONSTANT CX_csa1_2 : t_csa1_2 := ( OTHERS=>CX_scl2);
  CONSTANT C0_csa1_2 : t_csa1_2 := ( OTHERS=>C0_scl2);
  CONSTANT C1_csa1_2 : t_csa1_2 := ( OTHERS=>C1_scl2);
  CONSTANT C2_csa1_2 : t_csa1_2 := ( t_csa1_2'LEFT|t_csa1_2'RIGHT=>C2_scl2,
                                     OTHERS                      =>C0_scl2);
  
  CONSTANT CX_csa1_3 : t_csa1_3 := ( OTHERS=>CX_scl3);
  CONSTANT C0_csa1_3 : t_csa1_3 := ( OTHERS=>C0_scl3);
  CONSTANT C1_csa1_3 : t_csa1_3 := ( OTHERS=>C1_scl3);
  CONSTANT C2_csa1_3 : t_csa1_3 := ( t_csa1_3'LEFT|t_csa1_3'RIGHT=>C2_scl3,
                                     OTHERS                      =>C0_scl3);
  
  CONSTANT CX_csa1_4 : t_csa1_4 := ( OTHERS=>CX_scl4);
  CONSTANT C0_csa1_4 : t_csa1_4 := ( OTHERS=>C0_scl4);
  CONSTANT C1_csa1_4 : t_csa1_4 := ( OTHERS=>C1_scl4);
  CONSTANT C2_csa1_4 : t_csa1_4 := ( t_csa1_4'LEFT|t_csa1_4'RIGHT=>C2_scl4,
                                     OTHERS                      =>C0_scl4);
--
  CONSTANT CX_csa2_1 : t_csa2_1 ;
  CONSTANT C0_csa2_1 : t_csa2_1 ;
  CONSTANT C1_csa2_1 : t_csa2_1 ;
  CONSTANT C2_csa2_1 : t_csa2_1 ;
  
  CONSTANT CX_csa3_1 : t_csa3_1 ;
  CONSTANT C0_csa3_1 : t_csa3_1 ;
  CONSTANT C1_csa3_1 : t_csa3_1 ;
  CONSTANT C2_csa3_1 : t_csa3_1 ;
  
  CONSTANT CX_csa4_1 : t_csa4_1 ;
  CONSTANT C0_csa4_1 : t_csa4_1 ;
  CONSTANT C1_csa4_1 : t_csa4_1 ;
  CONSTANT C2_csa4_1 : t_csa4_1 ;
--
  CONSTANT CX_cca1_1 : t_cca1_1 := ( OTHERS=>CX_scre_1 );
  CONSTANT C0_cca1_1 : t_cca1_1 := ( OTHERS=>C0_scre_1 );
  CONSTANT C1_cca1_1 : t_cca1_1 := ( OTHERS=>C1_scre_1 );
  CONSTANT C2_cca1_1 : t_cca1_1 := ( C2_scre_1, C0_scre_1, C0_scre_1, C0_scre_1,
                                     C0_scre_1, C0_scre_1, C0_scre_1, C2_scre_1 ); 
  CONSTANT CX_cca1_2 : t_cca1_2 := ( OTHERS=>CX_csa1_1 );
  CONSTANT C0_cca1_2 : t_cca1_2 := ( OTHERS=>C0_csa1_1 );
  CONSTANT C1_cca1_2 : t_cca1_2 := ( OTHERS=>C1_csa1_1 );
  CONSTANT C2_cca1_2 : t_cca1_2 := ( C2_csa1_1, C0_csa1_1, C0_csa1_1, C2_csa1_1 ); 
  CONSTANT CX_cca1_3 : t_cca1_3 := ( OTHERS=>CX_cca1_2 );
  CONSTANT C0_cca1_3 : t_cca1_3 := ( OTHERS=>C0_cca1_2 );
  CONSTANT C1_cca1_3 : t_cca1_3 := ( OTHERS=>C1_cca1_2 );
  CONSTANT C2_cca1_3 : t_cca1_3 := ( C2_cca1_2, C0_cca1_2, C0_cca1_2, C2_cca1_2 ); 
  CONSTANT CX_cca1_4 : t_cca1_4 := ( OTHERS=>CX_cca1_3 );
  CONSTANT C0_cca1_4 : t_cca1_4 := ( OTHERS=>C0_cca1_3 );
  CONSTANT C1_cca1_4 : t_cca1_4 := ( OTHERS=>C1_cca1_3 );
  CONSTANT C2_cca1_4 : t_cca1_4 := ( C2_cca1_3, C0_cca1_3, C0_cca1_3, C2_cca1_3 ); 
  CONSTANT CX_cca2_1 : t_cca2_1 ;
  CONSTANT C0_cca2_1 : t_cca2_1 ;
  CONSTANT C1_cca2_1 : t_cca2_1 ;
  CONSTANT C2_cca2_1 : t_cca2_1 ;
--
  CONSTANT CX_cca2_2 : t_cca2_2 ;
  CONSTANT C0_cca2_2 : t_cca2_2 ;
  CONSTANT C1_cca2_2 : t_cca2_2 ;
  CONSTANT C2_cca2_2 : t_cca2_2 ;
  
  CONSTANT CX_cca3_1 : t_cca3_1 ;
  CONSTANT C0_cca3_1 : t_cca3_1 ;
  CONSTANT C1_cca3_1 : t_cca3_1 ;
  CONSTANT C2_cca3_1 : t_cca3_1 ;
--
  CONSTANT CX_cca3_2 : t_cca3_2 ;
  CONSTANT C0_cca3_2 : t_cca3_2 ;
  CONSTANT C1_cca3_2 : t_cca3_2 ;
  CONSTANT C2_cca3_2 : t_cca3_2 ;
  
  CONSTANT CX_cmre_1 : t_cmre_1 := ( CX_csa1_1, CX_scre_1 );
  CONSTANT C0_cmre_1 : t_cmre_1 := ( C0_csa1_1, C0_scre_1 );
  CONSTANT C1_cmre_1 : t_cmre_1 := ( C1_csa1_1, C1_scre_1 );
  CONSTANT C2_cmre_1 : t_cmre_1 := ( C2_csa1_1, C0_scre_1 );
  
  CONSTANT CX_cmre_2 : t_cmre_2 := ( OTHERS=>CX_csa1_1 );
  CONSTANT C0_cmre_2 : t_cmre_2 := ( OTHERS=>C0_csa1_1 );
  CONSTANT C1_cmre_2 : t_cmre_2 := ( OTHERS=>C1_csa1_1 );
  CONSTANT C2_cmre_2 : t_cmre_2 := ( left|right=>C2_csa1_1, OTHERS=>C0_csa1_1 );
  
  CONSTANT CX_cca1_7 : t_cca1_7 := ( OTHERS=>CX_cmre_2 );
  CONSTANT C0_cca1_7 : t_cca1_7 := ( OTHERS=>C0_cmre_2 );
  CONSTANT C1_cca1_7 : t_cca1_7 := ( OTHERS=>C1_cmre_2 );
  CONSTANT C2_cca1_7 : t_cca1_7 := ( C2_cmre_2, C0_cmre_2, C0_cmre_2, C2_cmre_2 ); 
  CONSTANT CX_cmre_3 : t_cmre_3 := ( OTHERS=>CX_cca1_7 );
  CONSTANT C0_cmre_3 : t_cmre_3 := ( OTHERS=>C0_cca1_7 );
  CONSTANT C1_cmre_3 : t_cmre_3 := ( OTHERS=>C1_cca1_7 );
  CONSTANT C2_cmre_3 : t_cmre_3 := ( left|right=>C2_cca1_7, OTHERS=>C0_cca1_7 );
  
-- --------------------------------------------------------------------------------------------
--      Functions for mapping from integer test values to/from values of the Test types
-- --------------------------------------------------------------------------------------------
  FUNCTION val_t ( i : INTEGER ) RETURN st_scl1;
  FUNCTION val_t ( i : INTEGER ) RETURN TIME;
  FUNCTION val_t ( i : INTEGER ) RETURN st_scl3;
  FUNCTION val_t ( i : INTEGER ) RETURN st_scl4;
  FUNCTION val_t ( i : INTEGER ) RETURN t_scre_1;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa1_1;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa1_2;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa1_3;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa1_4;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa2_1;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa3_1;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa4_1;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_1;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_2;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_3;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_4;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca2_1;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca2_2;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca3_1;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca3_2;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cmre_1;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cmre_2;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_7;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cmre_3;
  
  FUNCTION val_i ( i : st_scl1 )  RETURN INTEGER;
  FUNCTION val_i ( i : TIME )     RETURN INTEGER;
  FUNCTION val_i ( i : st_scl3 )  RETURN INTEGER;
  FUNCTION val_i ( i : st_scl4 )  RETURN INTEGER;
  FUNCTION val_i ( i : t_scre_1 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_csa1_1 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_csa1_2 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_csa1_3 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_csa1_4 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_csa2_1 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_csa3_1 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_csa4_1 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cca1_1 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cca1_2 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cca1_3 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cca1_4 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cca2_1 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cca2_2 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cca3_1 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cca3_2 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cmre_1 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cmre_2 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cca1_7 ) RETURN INTEGER;
  FUNCTION val_i ( i : t_cmre_3 ) RETURN INTEGER;
  
  FUNCTION val_s ( i : st_scl1 )  RETURN STRING;
  FUNCTION val_s ( i : TIME )     RETURN STRING;
  FUNCTION val_s ( i : st_scl3 )  RETURN STRING;
  FUNCTION val_s ( i : st_scl4 )  RETURN STRING;
  FUNCTION val_s ( i : t_scre_1 ) RETURN STRING;
  FUNCTION val_s ( i : t_csa1_1 ) RETURN STRING;
  FUNCTION val_s ( i : t_csa1_2 ) RETURN STRING;
  FUNCTION val_s ( i : t_csa1_3 ) RETURN STRING;
  FUNCTION val_s ( i : t_csa1_4 ) RETURN STRING;
  FUNCTION val_s ( i : t_csa2_1 ) RETURN STRING;
  FUNCTION val_s ( i : t_csa3_1 ) RETURN STRING;
  FUNCTION val_s ( i : t_csa4_1 ) RETURN STRING;
  FUNCTION val_s ( i : t_cca1_1 ) RETURN STRING;
  FUNCTION val_s ( i : t_cca1_2 ) RETURN STRING;
  FUNCTION val_s ( i : t_cca1_3 ) RETURN STRING;
  FUNCTION val_s ( i : t_cca1_4 ) RETURN STRING;
  FUNCTION val_s ( i : t_cca2_1 ) RETURN STRING;
  FUNCTION val_s ( i : t_cca2_2 ) RETURN STRING;
  FUNCTION val_s ( i : t_cca3_1 ) RETURN STRING;
  FUNCTION val_s ( i : t_cca3_2 ) RETURN STRING;
  FUNCTION val_s ( i : t_cmre_1 ) RETURN STRING;
  FUNCTION val_s ( i : t_cmre_2 ) RETURN STRING;
  FUNCTION val_s ( i : t_cca1_7 ) RETURN STRING;
  FUNCTION val_s ( i : t_cmre_3 ) RETURN STRING;
  
END;

PACKAGE BODY c03s03b00x00p03n04i00521pkg IS
  
  CONSTANT CX_csa2_1 : t_csa2_1 := F_csa2_1 ( CX_scl1, CX_scl1 );
  CONSTANT C0_csa2_1 : t_csa2_1 := F_csa2_1 ( C0_scl1, C0_scl1 );
  CONSTANT C1_csa2_1 : t_csa2_1 := F_csa2_1 ( C1_scl1, C1_scl1 );
  CONSTANT C2_csa2_1 : t_csa2_1 := F_csa2_1 ( C0_scl1, C2_scl1 );
  
  CONSTANT CX_csa3_1 : t_csa3_1 := F_csa3_1 ( CX_scl1, CX_scl1 );
  CONSTANT C0_csa3_1 : t_csa3_1 := F_csa3_1 ( C0_scl1, C0_scl1 );
  CONSTANT C1_csa3_1 : t_csa3_1 := F_csa3_1 ( C1_scl1, C1_scl1 );
  CONSTANT C2_csa3_1 : t_csa3_1 := F_csa3_1 ( C0_scl1, C2_scl1 );
  
  CONSTANT CX_csa4_1 : t_csa4_1 := F_csa4_1 ( CX_scl1, CX_scl1 );
  CONSTANT C0_csa4_1 : t_csa4_1 := F_csa4_1 ( C0_scl1, C0_scl1 );
  CONSTANT C1_csa4_1 : t_csa4_1 := F_csa4_1 ( C1_scl1, C1_scl1 );
  CONSTANT C2_csa4_1 : t_csa4_1 := F_csa4_1 ( C0_scl1, C2_scl1 );
  
  CONSTANT CX_cca2_1 : t_cca2_1 := ( OTHERS=>CX_csa2_1 );
  CONSTANT C0_cca2_1 : t_cca2_1 := ( OTHERS=>C0_csa2_1 );
  CONSTANT C1_cca2_1 : t_cca2_1 := ( OTHERS=>C1_csa2_1 );
  CONSTANT C2_cca2_1 : t_cca2_1 := ( C2_csa2_1, C0_csa2_1, C0_csa2_1, C2_csa2_1 ); 
  CONSTANT CX_cca2_2 : t_cca2_2 := F_cca2_2 ( CX_csa2_1, CX_csa2_1 );
  CONSTANT C0_cca2_2 : t_cca2_2 := F_cca2_2 ( C0_csa2_1, C0_csa2_1 );
  CONSTANT C1_cca2_2 : t_cca2_2 := F_cca2_2 ( C1_csa2_1, C1_csa2_1 );
  CONSTANT C2_cca2_2 : t_cca2_2 := F_cca2_2 ( C0_csa2_1, C2_csa2_1 );
  
  CONSTANT CX_cca3_1 : t_cca3_1 := F_cca3_1 ( CX_csa1_1, CX_csa1_1 );
  CONSTANT C0_cca3_1 : t_cca3_1 := F_cca3_1 ( C0_csa1_1, C0_csa1_1 );
  CONSTANT C1_cca3_1 : t_cca3_1 := F_cca3_1 ( C1_csa1_1, C1_csa1_1 );
  CONSTANT C2_cca3_1 : t_cca3_1 := F_cca3_1 ( C0_csa1_1, C2_csa1_1 );
  
  CONSTANT CX_cca3_2 : t_cca3_2 := ( OTHERS=>CX_csa3_1 );
  CONSTANT C0_cca3_2 : t_cca3_2 := ( OTHERS=>C0_csa3_1 );
  CONSTANT C1_cca3_2 : t_cca3_2 := ( OTHERS=>C1_csa3_1 );
  CONSTANT C2_cca3_2 : t_cca3_2 := ( C2_csa3_1, C0_csa3_1, C0_csa3_1, C2_csa3_1 ); 
--
-- Functions to provide values for multi-dimensional composites
--
  FUNCTION F_csa2_1 ( v0,v2 : IN st_scl1 ) RETURN t_csa2_1 IS
    VARIABLE res : t_csa2_1;
  BEGIN
    FOR i IN res'RANGE(1) LOOP
      FOR j IN res'RANGE(2) LOOP
        res(i,j) := v0;
      END LOOP;
    END LOOP;
    res(res'left (1),res'left (2)) := v2;
    res(res'left (1),res'right(2)) := v2;
    res(res'right(1),res'left (2)) := v2;
    res(res'right(1),res'right(2)) := v2;
    RETURN res;
  END;
  
  FUNCTION F_csa3_1 ( v0,v2 : IN st_scl1 ) RETURN t_csa3_1 IS
    VARIABLE res : t_csa3_1;
  BEGIN
    FOR i IN res'RANGE(1) LOOP
      FOR j IN res'RANGE(2) LOOP
        FOR k IN res'RANGE(3) LOOP
          res(i,j,k) := v0;
        END LOOP;
      END LOOP;
    END LOOP;
    res(res'left (1),res'left (2),res'left (3)) := v2;
    res(res'right(1),res'left (2),res'left (3)) := v2;
    res(res'left (1),res'right(2),res'left (3)) := v2;
    res(res'right(1),res'right(2),res'left (3)) := v2;
    res(res'left (1),res'left (2),res'right(3)) := v2;
    res(res'right(1),res'left (2),res'right(3)) := v2;
    res(res'left (1),res'right(2),res'right(3)) := v2;
    res(res'right(1),res'right(2),res'right(3)) := v2;
    RETURN res;
  END;
  
  FUNCTION F_csa4_1 ( v0,v2 : IN st_scl1 ) RETURN t_csa4_1 IS
    VARIABLE res : t_csa4_1;
  BEGIN
    FOR i IN res'RANGE(1) LOOP
      FOR j IN res'RANGE(2) LOOP
        FOR k IN res'RANGE(3) LOOP
          FOR l IN res'RANGE(4) LOOP
            res(i,j,k,l) := v0;
          END LOOP;
        END LOOP;
      END LOOP;
    END LOOP;
    res(res'left (1),res'left (2),res'left (3),res'left (4)) := v2;
    res(res'right(1),res'left (2),res'left (3),res'left (4)) := v2;
    res(res'left (1),res'right(2),res'left (3),res'left (4)) := v2;
    res(res'right(1),res'right(2),res'left (3),res'left (4)) := v2;
    res(res'left (1),res'left (2),res'right(3),res'left (4)) := v2;
    res(res'right(1),res'left (2),res'right(3),res'left (4)) := v2;
    res(res'left (1),res'right(2),res'right(3),res'left (4)) := v2;
    res(res'right(1),res'right(2),res'right(3),res'left (4)) := v2;
    res(res'left (1),res'left (2),res'left (3),res'right(4)) := v2;
    res(res'right(1),res'left (2),res'left (3),res'right(4)) := v2;
    res(res'left (1),res'right(2),res'left (3),res'right(4)) := v2;
    res(res'right(1),res'right(2),res'left (3),res'right(4)) := v2;
    res(res'left (1),res'left (2),res'right(3),res'right(4)) := v2;
    res(res'right(1),res'left (2),res'right(3),res'right(4)) := v2;
    res(res'left (1),res'right(2),res'right(3),res'right(4)) := v2;
    res(res'right(1),res'right(2),res'right(3),res'right(4)) := v2;
    RETURN res;
  END;
  
  FUNCTION F_cca2_2 ( v0,v2 : IN t_csa2_1 ) RETURN t_cca2_2 IS
    VARIABLE res : t_cca2_2;
  BEGIN
    FOR i IN res'RANGE(1) LOOP
      FOR j IN res'RANGE(2) LOOP
        res(i,j) := v0;
      END LOOP;
    END LOOP;
    res(res'left (1),res'left (2)) := v2;
    res(res'left (1),res'right(2)) := v2;
    res(res'right(1),res'left (2)) := v2;
    res(res'right(1),res'right(2)) := v2;
    RETURN res;
  END;
  
  FUNCTION F_cca3_1 ( v0,v2 : IN t_csa1_1 ) RETURN t_cca3_1 IS
    VARIABLE res : t_cca3_1;
  BEGIN
    FOR i IN res'RANGE(1) LOOP
      FOR j IN res'RANGE(2) LOOP
        FOR k IN res'RANGE(3) LOOP
          res(i,j,k) := v0;
        END LOOP;
      END LOOP;
    END LOOP;
    res(res'left (1),res'left (2),res'left (3)) := v2;
    res(res'right(1),res'left (2),res'left (3)) := v2;
    res(res'left (1),res'right(2),res'left (3)) := v2;
    res(res'right(1),res'right(2),res'left (3)) := v2;
    res(res'left (1),res'left (2),res'right(3)) := v2;
    res(res'right(1),res'left (2),res'right(3)) := v2;
    res(res'left (1),res'right(2),res'right(3)) := v2;
    res(res'right(1),res'right(2),res'right(3)) := v2;
    RETURN res;
  END;
  
--
--          Resolution Functions
--
  FUNCTION rf_scre_1 ( v: t_scre_1_vct ) RETURN t_scre_1 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_scre_1;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_csa1_1 ( v: t_csa1_1_vct ) RETURN t_csa1_1 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_csa1_1;
    ELSE RETURN v(1);
    END IF;
  END;
  FUNCTION rf_csa1_2 ( v: t_csa1_2_vct ) RETURN t_csa1_2 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_csa1_2;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_csa1_3 ( v: t_csa1_3_vct ) RETURN t_csa1_3 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_csa1_3;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_csa1_4 ( v: t_csa1_4_vct ) RETURN t_csa1_4 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_csa1_4;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_csa2_1 ( v: t_csa2_1_vct ) RETURN t_csa2_1 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_csa2_1;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_csa3_1 ( v: t_csa3_1_vct ) RETURN t_csa3_1 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_csa3_1;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_csa4_1 ( v: t_csa4_1_vct ) RETURN t_csa4_1 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_csa4_1;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cca1_1 ( v: t_cca1_1_vct ) RETURN t_cca1_1 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cca1_1;
    ELSE RETURN v(1);
    END IF;
  END;

  FUNCTION rf_cca1_2 ( v: t_cca1_2_vct ) RETURN t_cca1_2 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cca1_2;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cca1_3 ( v: t_cca1_3_vct ) RETURN t_cca1_3 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cca1_3;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cca1_4 ( v: t_cca1_4_vct ) RETURN t_cca1_4 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cca1_4;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cca2_1 ( v: t_cca2_1_vct ) RETURN t_cca2_1 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cca2_1;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cca2_2 ( v: t_cca2_2_vct ) RETURN t_cca2_2 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cca2_2;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cca3_1 ( v: t_cca3_1_vct ) RETURN t_cca3_1 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cca3_1;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cca3_2 ( v: t_cca3_2_vct ) RETURN t_cca3_2 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cca3_2;
    ELSE RETURN v(1);
    END IF;
  END;

  FUNCTION rf_cmre_1 ( v: t_cmre_1_vct ) RETURN t_cmre_1 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cmre_1;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cmre_2 ( v: t_cmre_2_vct ) RETURN t_cmre_2 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cmre_2;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cca1_7 ( v: t_cca1_7_vct ) RETURN t_cca1_7 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cca1_7;
    ELSE RETURN v(1);
    END IF;
  END;
  
  FUNCTION rf_cmre_3 ( v: t_cmre_3_vct ) RETURN t_cmre_3 IS
  BEGIN
    IF v'LENGTH=0
    THEN RETURN CX_cmre_3;
    ELSE RETURN v(1);
    END IF;
  END;
-- 
-- 
  FUNCTION val_t ( i : INTEGER ) RETURN st_scl1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_scl1; END IF;
    IF i = 1 THEN RETURN C1_scl1; END IF;
    IF i = 2 THEN RETURN C2_scl1; END IF;
    RETURN CX_scl1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN TIME IS
  BEGIN
    IF i = 0 THEN RETURN C0_scl2; END IF;
    IF i = 1 THEN RETURN C1_scl2; END IF;
    IF i = 2 THEN RETURN C2_scl2; END IF;
    RETURN CX_scl2;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN st_scl3 IS
  BEGIN
    IF i = 0 THEN RETURN C0_scl3; END IF;
    IF i = 1 THEN RETURN C1_scl3; END IF;
    IF i = 2 THEN RETURN C2_scl3; END IF;
    RETURN CX_scl3;
  END;

  FUNCTION val_t ( i : INTEGER ) RETURN st_scl4 IS
  BEGIN
    IF i = 0 THEN RETURN C0_scl4; END IF;
    IF i = 1 THEN RETURN C1_scl4; END IF;
    IF i = 2 THEN RETURN C2_scl4; END IF;
    RETURN CX_scl4;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_scre_1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_scre_1; END IF;
    IF i = 1 THEN RETURN C1_scre_1; END IF;
    IF i = 2 THEN RETURN C2_scre_1; END IF;
    RETURN CX_scre_1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa1_1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_csa1_1; END IF;
    IF i = 1 THEN RETURN C1_csa1_1; END IF;
    IF i = 2 THEN RETURN C2_csa1_1; END IF;
    RETURN CX_csa1_1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa1_2 IS
  BEGIN
    IF i = 0 THEN RETURN C0_csa1_2; END IF;
    IF i = 1 THEN RETURN C1_csa1_2; END IF;
    IF i = 2 THEN RETURN C2_csa1_2; END IF;
    RETURN CX_csa1_2;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa1_3 IS
  BEGIN
    IF i = 0 THEN RETURN C0_csa1_3; END IF;
    IF i = 1 THEN RETURN C1_csa1_3; END IF;
    IF i = 2 THEN RETURN C2_csa1_3; END IF;
    RETURN CX_csa1_3;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa1_4 IS
  BEGIN
    IF i = 0 THEN RETURN C0_csa1_4; END IF;
    IF i = 1 THEN RETURN C1_csa1_4; END IF;
    IF i = 2 THEN RETURN C2_csa1_4; END IF;
    RETURN CX_csa1_4;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa2_1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_csa2_1; END IF;
    IF i = 1 THEN RETURN C1_csa2_1; END IF;
    IF i = 2 THEN RETURN C2_csa2_1; END IF;
    RETURN CX_csa2_1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa3_1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_csa3_1; END IF;
    IF i = 1 THEN RETURN C1_csa3_1; END IF;
    IF i = 2 THEN RETURN C2_csa3_1; END IF;
    RETURN CX_csa3_1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_csa4_1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_csa4_1; END IF;
    IF i = 1 THEN RETURN C1_csa4_1; END IF;
    IF i = 2 THEN RETURN C2_csa4_1; END IF;
    RETURN CX_csa4_1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cca1_1; END IF;
    IF i = 1 THEN RETURN C1_cca1_1; END IF;
    IF i = 2 THEN RETURN C2_cca1_1; END IF;
    RETURN CX_cca1_1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_2 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cca1_2; END IF;
    IF i = 1 THEN RETURN C1_cca1_2; END IF;
    IF i = 2 THEN RETURN C2_cca1_2; END IF;
    RETURN CX_cca1_2;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_3 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cca1_3; END IF;
    IF i = 1 THEN RETURN C1_cca1_3; END IF;
    IF i = 2 THEN RETURN C2_cca1_3; END IF;
    RETURN CX_cca1_3;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_4 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cca1_4; END IF;
    IF i = 1 THEN RETURN C1_cca1_4; END IF;
    IF i = 2 THEN RETURN C2_cca1_4; END IF;
    RETURN CX_cca1_4;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca2_1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cca2_1; END IF;
    IF i = 1 THEN RETURN C1_cca2_1; END IF;
    IF i = 2 THEN RETURN C2_cca2_1; END IF;
    RETURN CX_cca2_1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca2_2 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cca2_2; END IF;
    IF i = 1 THEN RETURN C1_cca2_2; END IF;
    IF i = 2 THEN RETURN C2_cca2_2; END IF;
    RETURN CX_cca2_2;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca3_1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cca3_1; END IF;
    IF i = 1 THEN RETURN C1_cca3_1; END IF;
    IF i = 2 THEN RETURN C2_cca3_1; END IF;
    RETURN CX_cca3_1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca3_2 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cca3_2; END IF;
    IF i = 1 THEN RETURN C1_cca3_2; END IF;
    IF i = 2 THEN RETURN C2_cca3_2; END IF;
    RETURN CX_cca3_2;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cmre_1 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cmre_1; END IF;
    IF i = 1 THEN RETURN C1_cmre_1; END IF;
    IF i = 2 THEN RETURN C2_cmre_1; END IF;
    RETURN CX_cmre_1;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cmre_2 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cmre_2; END IF;
    IF i = 1 THEN RETURN C1_cmre_2; END IF;
    IF i = 2 THEN RETURN C2_cmre_2; END IF;
    RETURN CX_cmre_2;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cca1_7 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cca1_7; END IF;
    IF i = 1 THEN RETURN C1_cca1_7; END IF;
    IF i = 2 THEN RETURN C2_cca1_7; END IF;
    RETURN CX_cca1_7;
  END;
  FUNCTION val_t ( i : INTEGER ) RETURN t_cmre_3 IS
  BEGIN
    IF i = 0 THEN RETURN C0_cmre_3; END IF;
    IF i = 1 THEN RETURN C1_cmre_3; END IF;
    IF i = 2 THEN RETURN C2_cmre_3; END IF;
    RETURN CX_cmre_3;
  END;
--
-- 
  FUNCTION val_i ( i : st_scl1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_scl1 THEN RETURN 0; END IF;
    IF i = C1_scl1 THEN RETURN 1; END IF;
    IF i = C2_scl1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : TIME ) RETURN INTEGER IS
  BEGIN
    IF i = C0_scl2 THEN RETURN 0; END IF;
    IF i = C1_scl2 THEN RETURN 1; END IF;
    IF i = C2_scl2 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : st_scl3 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_scl3 THEN RETURN 0; END IF;
    IF i = C1_scl3 THEN RETURN 1; END IF;
    IF i = C2_scl3 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : st_scl4 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_scl4 THEN RETURN 0; END IF;
    IF i = C1_scl4 THEN RETURN 1; END IF;
    IF i = C2_scl4 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_scre_1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_scre_1 THEN RETURN 0; END IF;
    IF i = C1_scre_1 THEN RETURN 1; END IF;
    IF i = C2_scre_1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_csa1_1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_csa1_1 THEN RETURN 0; END IF;
    IF i = C1_csa1_1 THEN RETURN 1; END IF;
    IF i = C2_csa1_1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_csa1_2 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_csa1_2 THEN RETURN 0; END IF;
    IF i = C1_csa1_2 THEN RETURN 1; END IF;
    IF i = C2_csa1_2 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_csa1_3 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_csa1_3 THEN RETURN 0; END IF;
    IF i = C1_csa1_3 THEN RETURN 1; END IF;
    IF i = C2_csa1_3 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_csa1_4 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_csa1_4 THEN RETURN 0; END IF;
    IF i = C1_csa1_4 THEN RETURN 1; END IF;
    IF i = C2_csa1_4 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_csa2_1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_csa2_1 THEN RETURN 0; END IF;
    IF i = C1_csa2_1 THEN RETURN 1; END IF;
    IF i = C2_csa2_1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_csa3_1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_csa3_1 THEN RETURN 0; END IF;
    IF i = C1_csa3_1 THEN RETURN 1; END IF;
    IF i = C2_csa3_1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_csa4_1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_csa4_1 THEN RETURN 0; END IF;
    IF i = C1_csa4_1 THEN RETURN 1; END IF;
    IF i = C2_csa4_1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cca1_1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cca1_1 THEN RETURN 0; END IF;
    IF i = C1_cca1_1 THEN RETURN 1; END IF;
    IF i = C2_cca1_1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cca1_2 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cca1_2 THEN RETURN 0; END IF;
    IF i = C1_cca1_2 THEN RETURN 1; END IF;
    IF i = C2_cca1_2 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cca1_3 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cca1_3 THEN RETURN 0; END IF;
    IF i = C1_cca1_3 THEN RETURN 1; END IF;
    IF i = C2_cca1_3 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cca1_4 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cca1_4 THEN RETURN 0; END IF;
    IF i = C1_cca1_4 THEN RETURN 1; END IF;
    IF i = C2_cca1_4 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cca2_1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cca2_1 THEN RETURN 0; END IF;
    IF i = C1_cca2_1 THEN RETURN 1; END IF;
    IF i = C2_cca2_1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cca2_2 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cca2_2 THEN RETURN 0; END IF;
    IF i = C1_cca2_2 THEN RETURN 1; END IF;
    IF i = C2_cca2_2 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cca3_1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cca3_1 THEN RETURN 0; END IF;
    IF i = C1_cca3_1 THEN RETURN 1; END IF;
    IF i = C2_cca3_1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cca3_2 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cca3_2 THEN RETURN 0; END IF;
    IF i = C1_cca3_2 THEN RETURN 1; END IF;
    IF i = C2_cca3_2 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cmre_1 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cmre_1 THEN RETURN 0; END IF;
    IF i = C1_cmre_1 THEN RETURN 1; END IF;
    IF i = C2_cmre_1 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cmre_2 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cmre_2 THEN RETURN 0; END IF;
    IF i = C1_cmre_2 THEN RETURN 1; END IF;
    IF i = C2_cmre_2 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cca1_7 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cca1_7 THEN RETURN 0; END IF;
    IF i = C1_cca1_7 THEN RETURN 1; END IF;
    IF i = C2_cca1_7 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  FUNCTION val_i ( i : t_cmre_3 ) RETURN INTEGER IS
  BEGIN
    IF i = C0_cmre_3 THEN RETURN 0; END IF;
    IF i = C1_cmre_3 THEN RETURN 1; END IF;
    IF i = C2_cmre_3 THEN RETURN 2; END IF;
    RETURN -1;
  END;
  
  FUNCTION val_s ( i : st_scl1 ) RETURN STRING IS
  BEGIN
    IF i = C0_scl1 THEN RETURN "C0_scl1"; END IF;
    IF i = C1_scl1 THEN RETURN "C1_scl1"; END IF;
    IF i = C2_scl1 THEN RETURN "C2_scl1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : TIME ) RETURN STRING IS
  BEGIN
    IF i = C0_scl2 THEN RETURN "C0_scl2"; END IF;
    IF i = C1_scl2 THEN RETURN "C1_scl2"; END IF;
    IF i = C2_scl2 THEN RETURN "C2_scl2"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : st_scl3 ) RETURN STRING IS
  BEGIN
    IF i = C0_scl3 THEN RETURN "C0_scl3"; END IF;
    IF i = C1_scl3 THEN RETURN "C1_scl3"; END IF;
    IF i = C2_scl3 THEN RETURN "C2_scl3"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : st_scl4 ) RETURN STRING IS
  BEGIN
    IF i = C0_scl4 THEN RETURN "C0_scl4"; END IF;
    IF i = C1_scl4 THEN RETURN "C1_scl4"; END IF;
    IF i = C2_scl4 THEN RETURN "C2_scl4"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_scre_1 ) RETURN STRING IS
  BEGIN
    IF i = C0_scre_1 THEN RETURN "C0_scre_1"; END IF;
    IF i = C1_scre_1 THEN RETURN "C1_scre_1"; END IF;
    IF i = C2_scre_1 THEN RETURN "C2_scre_1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_csa1_1 ) RETURN STRING IS
  BEGIN
    IF i = C0_csa1_1 THEN RETURN "C0_csa1_1"; END IF;
    IF i = C1_csa1_1 THEN RETURN "C1_csa1_1"; END IF;
    IF i = C2_csa1_1 THEN RETURN "C2_csa1_1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_csa1_2 ) RETURN STRING IS
  BEGIN
    IF i = C0_csa1_2 THEN RETURN "C0_csa1_2"; END IF;
    IF i = C1_csa1_2 THEN RETURN "C1_csa1_2"; END IF;
    IF i = C2_csa1_2 THEN RETURN "C2_csa1_2"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_csa1_3 ) RETURN STRING IS
  BEGIN
    IF i = C0_csa1_3 THEN RETURN "C0_csa1_3"; END IF;
    IF i = C1_csa1_3 THEN RETURN "C1_csa1_3"; END IF;
    IF i = C2_csa1_3 THEN RETURN "C2_csa1_3"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_csa1_4 ) RETURN STRING IS
  BEGIN
    IF i = C0_csa1_4 THEN RETURN "C0_csa1_4"; END IF;
    IF i = C1_csa1_4 THEN RETURN "C1_csa1_4"; END IF;
    IF i = C2_csa1_4 THEN RETURN "C2_csa1_4"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_csa2_1 ) RETURN STRING IS
  BEGIN
    IF i = C0_csa2_1 THEN RETURN "C0_csa2_1"; END IF;
    IF i = C1_csa2_1 THEN RETURN "C1_csa2_1"; END IF;
    IF i = C2_csa2_1 THEN RETURN "C2_csa2_1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_csa3_1 ) RETURN STRING IS
  BEGIN
    IF i = C0_csa3_1 THEN RETURN "C0_csa3_1"; END IF;
    IF i = C1_csa3_1 THEN RETURN "C1_csa3_1"; END IF;
    IF i = C2_csa3_1 THEN RETURN "C2_csa3_1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_csa4_1 ) RETURN STRING IS
  BEGIN
    IF i = C0_csa4_1 THEN RETURN "C0_csa4_1"; END IF;
    IF i = C1_csa4_1 THEN RETURN "C1_csa4_1"; END IF;
    IF i = C2_csa4_1 THEN RETURN "C2_csa4_1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cca1_1 ) RETURN STRING IS
  BEGIN
    IF i = C0_cca1_1 THEN RETURN "C0_cca1_1"; END IF;
    IF i = C1_cca1_1 THEN RETURN "C1_cca1_1"; END IF;
    IF i = C2_cca1_1 THEN RETURN "C2_cca1_1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cca1_2 ) RETURN STRING IS
  BEGIN
    IF i = C0_cca1_2 THEN RETURN "C0_cca1_2"; END IF;
    IF i = C1_cca1_2 THEN RETURN "C1_cca1_2"; END IF;
    IF i = C2_cca1_2 THEN RETURN "C2_cca1_2"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cca1_3 ) RETURN STRING IS
  BEGIN
    IF i = C0_cca1_3 THEN RETURN "C0_cca1_3"; END IF;
    IF i = C1_cca1_3 THEN RETURN "C1_cca1_3"; END IF;
    IF i = C2_cca1_3 THEN RETURN "C2_cca1_3"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cca1_4 ) RETURN STRING IS
  BEGIN
    IF i = C0_cca1_4 THEN RETURN "C0_cca1_4"; END IF;
    IF i = C1_cca1_4 THEN RETURN "C1_cca1_4"; END IF;
    IF i = C2_cca1_4 THEN RETURN "C2_cca1_4"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cca2_1 ) RETURN STRING IS
  BEGIN
    IF i = C0_cca2_1 THEN RETURN "C0_cca2_1"; END IF;
    IF i = C1_cca2_1 THEN RETURN "C1_cca2_1"; END IF;
    IF i = C2_cca2_1 THEN RETURN "C2_cca2_1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cca2_2 ) RETURN STRING IS
  BEGIN
    IF i = C0_cca2_2 THEN RETURN "C0_cca2_2"; END IF;
    IF i = C1_cca2_2 THEN RETURN "C1_cca2_2"; END IF;
    IF i = C2_cca2_2 THEN RETURN "C2_cca2_2"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cca3_1 ) RETURN STRING IS
  BEGIN
    IF i = C0_cca3_1 THEN RETURN "C0_cca3_1"; END IF;
    IF i = C1_cca3_1 THEN RETURN "C1_cca3_1"; END IF;
    IF i = C2_cca3_1 THEN RETURN "C2_cca3_1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cca3_2 ) RETURN STRING IS
  BEGIN
    IF i = C0_cca3_2 THEN RETURN "C0_cca3_2"; END IF;
    IF i = C1_cca3_2 THEN RETURN "C1_cca3_2"; END IF;
    IF i = C2_cca3_2 THEN RETURN "C2_cca3_2"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cmre_1 ) RETURN STRING IS
  BEGIN
    IF i = C0_cmre_1 THEN RETURN "C0_cmre_1"; END IF;
    IF i = C1_cmre_1 THEN RETURN "C1_cmre_1"; END IF;
    IF i = C2_cmre_1 THEN RETURN "C2_cmre_1"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cmre_2 ) RETURN STRING IS
  BEGIN
    IF i = C0_cmre_2 THEN RETURN "C0_cmre_2"; END IF;
    IF i = C1_cmre_2 THEN RETURN "C1_cmre_2"; END IF;
    IF i = C2_cmre_2 THEN RETURN "C2_cmre_2"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cca1_7 ) RETURN STRING IS
  BEGIN
    IF i = C0_cca1_7 THEN RETURN "C0_cca1_7"; END IF;
    IF i = C1_cca1_7 THEN RETURN "C1_cca1_7"; END IF;
    IF i = C2_cca1_7 THEN RETURN "C2_cca1_7"; END IF;
    RETURN "UNKNOWN";
  END;
  FUNCTION val_s ( i : t_cmre_3 ) RETURN STRING IS
  BEGIN
    IF i = C0_cmre_3 THEN RETURN "C0_cmre_3"; END IF;
    IF i = C1_cmre_3 THEN RETURN "C1_cmre_3"; END IF;
    IF i = C2_cmre_3 THEN RETURN "C2_cmre_3"; END IF;
    RETURN "UNKNOWN";
  END;
  
END c03s03b00x00p03n04i00521pkg;

USE   work.c03s03b00x00p03n04i00521pkg.ALL;
ENTITY c03s03b00x00p03n04i00521ent IS
END c03s03b00x00p03n04i00521ent;

ARCHITECTURE c03s03b00x00p03n04i00521arch OF c03s03b00x00p03n04i00521ent IS
--
--          Access type declarations
--
  TYPE at_usa2_1 IS ACCESS t_usa2_1 ;
  TYPE at_usa3_1 IS ACCESS t_usa3_1 ;
  TYPE at_usa4_1 IS ACCESS t_usa4_1 ;
  TYPE at_csa2_1 IS ACCESS t_csa2_1 ;
  TYPE at_csa3_1 IS ACCESS t_csa3_1 ;
  TYPE at_csa4_1 IS ACCESS t_csa4_1 ;
  TYPE at_cca2_1 IS ACCESS t_cca2_1 ;
  TYPE at_cca2_2 IS ACCESS t_cca2_2 ;
  TYPE at_cca3_1 IS ACCESS t_cca3_1 ;
  TYPE at_cca3_2 IS ACCESS t_cca3_2 ;
--
--
BEGIN
  TESTING: PROCESS
--
--          ACCESS VARIABLE declarations
--
    VARIABLE AV0_usa2_1 : at_usa2_1 ;
    VARIABLE AV2_usa2_1 : at_usa2_1 ;
    VARIABLE AV0_usa3_1 : at_usa3_1 ;
    VARIABLE AV2_usa3_1 : at_usa3_1 ;
    VARIABLE AV0_usa4_1 : at_usa4_1 ;
    VARIABLE AV2_usa4_1 : at_usa4_1 ;
    VARIABLE AV0_csa2_1 : at_csa2_1 ;
    VARIABLE AV2_csa2_1 : at_csa2_1 ;
    VARIABLE AV0_csa3_1 : at_csa3_1 ;
    VARIABLE AV2_csa3_1 : at_csa3_1 ;
    VARIABLE AV0_csa4_1 : at_csa4_1 ;
    VARIABLE AV2_csa4_1 : at_csa4_1 ;
    VARIABLE AV0_cca2_1 : at_cca2_1 ;
    VARIABLE AV2_cca2_1 : at_cca2_1 ;
    VARIABLE AV0_cca2_2 : at_cca2_2 ;
    VARIABLE AV2_cca2_2 : at_cca2_2 ;
    VARIABLE AV0_cca3_1 : at_cca3_1 ;
    VARIABLE AV2_cca3_1 : at_cca3_1 ;
    VARIABLE AV0_cca3_2 : at_cca3_2 ;
    VARIABLE AV2_cca3_2 : at_cca3_2 ;
--
--
  BEGIN
--
--          Allocation of access values
--
    AV0_usa2_1 := NEW t_usa2_1 (                  st_ind2, st_ind1 ) ;
    AV0_usa3_1 := NEW t_usa3_1 (         st_ind3, st_ind2, st_ind1 ) ;
    AV0_usa4_1 := NEW t_usa4_1 (st_ind4, st_ind3, st_ind2, st_ind1 ) ;
    AV0_csa2_1 := NEW t_csa2_1 ;
    AV0_csa3_1 := NEW t_csa3_1 ;
    AV0_csa4_1 := NEW t_csa4_1 ;
    AV0_cca2_1 := NEW t_cca2_1 ;
    AV0_cca2_2 := NEW t_cca2_2 ;
    AV0_cca3_1 := NEW t_cca3_1 ;
    AV0_cca3_2 := NEW t_cca3_2 ;
---
    AV2_usa2_1 := NEW t_usa2_1 ' ( C2_csa2_1 ) ;
    AV2_usa3_1 := NEW t_usa3_1 ' ( C2_csa3_1 ) ;
    AV2_usa4_1 := NEW t_usa4_1 ' ( C2_csa4_1 ) ;
    AV2_csa2_1 := NEW t_csa2_1 ' ( C2_csa2_1 ) ;
    AV2_csa3_1 := NEW t_csa3_1 ' ( C2_csa3_1 ) ;
    AV2_csa4_1 := NEW t_csa4_1 ' ( C2_csa4_1 ) ;
    AV2_cca2_1 := NEW t_cca2_1 ' ( C2_cca2_1 ) ;
    AV2_cca2_2 := NEW t_cca2_2 ' ( C2_cca2_2 ) ;
    AV2_cca3_1 := NEW t_cca3_1 ' ( C2_cca3_1 ) ;
    AV2_cca3_2 := NEW t_cca3_2 ' ( C2_cca3_2 ) ;
--
--
    ASSERT AV0_usa2_1.all = C0_csa2_1  
      REPORT "Improper initialization of AV0_usa2_1" SEVERITY FAILURE;
    ASSERT AV2_usa2_1.all = C2_csa2_1  
      REPORT "Improper initialization of AV2_usa2_1" SEVERITY FAILURE;
    ASSERT AV0_usa3_1.all = C0_csa3_1  
      REPORT "Improper initialization of AV0_usa3_1" SEVERITY FAILURE;
    ASSERT AV2_usa3_1.all = C2_csa3_1  
      REPORT "Improper initialization of AV2_usa3_1" SEVERITY FAILURE;
    ASSERT AV0_usa4_1.all = C0_csa4_1  
      REPORT "Improper initialization of AV0_usa4_1" SEVERITY FAILURE;
    ASSERT AV2_usa4_1.all = C2_csa4_1  
      REPORT "Improper initialization of AV2_usa4_1" SEVERITY FAILURE;
    ASSERT AV0_csa2_1.all = C0_csa2_1  
      REPORT "Improper initialization of AV0_csa2_1" SEVERITY FAILURE;
    ASSERT AV2_csa2_1.all = C2_csa2_1  
      REPORT "Improper initialization of AV2_csa2_1" SEVERITY FAILURE;
    ASSERT AV0_csa3_1.all = C0_csa3_1  
      REPORT "Improper initialization of AV0_csa3_1" SEVERITY FAILURE;
    ASSERT AV2_csa3_1.all = C2_csa3_1  
      REPORT "Improper initialization of AV2_csa3_1" SEVERITY FAILURE;
    ASSERT AV0_csa4_1.all = C0_csa4_1  
      REPORT "Improper initialization of AV0_csa4_1" SEVERITY FAILURE;
    ASSERT AV2_csa4_1.all = C2_csa4_1  
      REPORT "Improper initialization of AV2_csa4_1" SEVERITY FAILURE;
    ASSERT AV0_cca2_1.all = C0_cca2_1  
      REPORT "Improper initialization of AV0_cca2_1" SEVERITY FAILURE;
    ASSERT AV2_cca2_1.all = C2_cca2_1  
      REPORT "Improper initialization of AV2_cca2_1" SEVERITY FAILURE;
    ASSERT AV0_cca2_2.all = C0_cca2_2  
      REPORT "Improper initialization of AV0_cca2_2" SEVERITY FAILURE;
    ASSERT AV2_cca2_2.all = C2_cca2_2  
      REPORT "Improper initialization of AV2_cca2_2" SEVERITY FAILURE;
    ASSERT AV0_cca3_1.all = C0_cca3_1  
      REPORT "Improper initialization of AV0_cca3_1" SEVERITY FAILURE;
    ASSERT AV2_cca3_1.all = C2_cca3_1  
      REPORT "Improper initialization of AV2_cca3_1" SEVERITY FAILURE;
    ASSERT AV0_cca3_2.all = C0_cca3_2  
      REPORT "Improper initialization of AV0_cca3_2" SEVERITY FAILURE;
    ASSERT AV2_cca3_2.all = C2_cca3_2  
      REPORT "Improper initialization of AV2_cca3_2" SEVERITY FAILURE;
--
--
    assert NOT( ( AV0_usa2_1.all = C0_csa2_1 ) 
                and ( AV2_usa2_1.all = C2_csa2_1 ) 
                and ( AV0_usa3_1.all = C0_csa3_1 ) 
                and ( AV2_usa3_1.all = C2_csa3_1 ) 
                and ( AV0_usa4_1.all = C0_csa4_1 ) 
                and ( AV2_usa4_1.all = C2_csa4_1 ) 
                and ( AV0_csa2_1.all = C0_csa2_1 ) 
                and ( AV2_csa2_1.all = C2_csa2_1 ) 
                and ( AV0_csa3_1.all = C0_csa3_1 ) 
                and ( AV2_csa3_1.all = C2_csa3_1 )) 
      report "***PASSED TEST: c03s03b00x00p03n04i00521"
      severity NOTE;
    assert  (   ( AV0_usa2_1.all = C0_csa2_1 ) 
                and ( AV2_usa2_1.all = C2_csa2_1 ) 
                and ( AV0_usa3_1.all = C0_csa3_1 ) 
                and ( AV2_usa3_1.all = C2_csa3_1 ) 
                and ( AV0_usa4_1.all = C0_csa4_1 ) 
                and ( AV2_usa4_1.all = C2_csa4_1 ) 
                and ( AV0_csa2_1.all = C0_csa2_1 ) 
                and ( AV2_csa2_1.all = C2_csa2_1 ) 
                and ( AV0_csa3_1.all = C0_csa3_1 ) 
                and ( AV2_csa3_1.all = C2_csa3_1 )) 
      report "***FAILED TEST: c03s03b00x00p03n04i00521 - Each access value designates an object of the subtype defined by the subtype indication of the access type definition."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s03b00x00p03n04i00521arch;
