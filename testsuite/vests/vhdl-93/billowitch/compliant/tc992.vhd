
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
-- $Id: tc992.vhd,v 1.2 2001-10-26 16:30:02 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

PACKAGE c06s03b00x00p08n01i00992pkg IS
--
--  This packages contains declarations of User attributes
--
-- ----------------------------------------------------------------------
--
  TYPE RESISTANCE IS RANGE 0 TO 1E9
    UNITS
      pf;
      nf = 1000 pf;
      mf = 1000 nf;
    END UNITS;
  
  TYPE t_logic IS (
    U,  D,
    Z0, Z1, ZDX, DZX, ZX,
    W0, W1, WZ0, WZ1, WDX, DWX, WZX, ZWX, WX,
    R0, R1, RW0, RW1, RZ0, RZ1, RDX, DRX, RZX, ZRX, RWX, WRX, RX,
    F0, F1, FR0, FR1, FW0, FW1, FZ0, FZ1, FDX, DFX, FZX, ZFX, FWX, WFX, FRX, RFX, FX
    );
--
--          Scalar types Declarations
--
  SUBTYPE st_scl1 IS BOOLEAN;
  SUBTYPE st_scl2 IS BIT;
  SUBTYPE st_scl3 IS CHARACTER;
  SUBTYPE st_scl4 IS INTEGER;
  SUBTYPE st_scl5 IS REAL;
  SUBTYPE st_scl6 IS TIME;
  SUBTYPE st_scl7 IS RESISTANCE;
  SUBTYPE st_scl8 IS t_logic;
--
--          character string types
--
  SUBTYPE st_str1 IS STRING;
  SUBTYPE st_str2 IS STRING (1 TO 4);
--
--          Scalar types with a range constraint
--
  SUBTYPE cst_scl1 IS BOOLEAN    RANGE  TRUE   TO      TRUE;
  SUBTYPE cst_scl2 IS BIT        RANGE   '0'   TO       '0';
  SUBTYPE cst_scl3 IS CHARACTER  RANGE   'a'   TO       'z';
  SUBTYPE cst_scl4 IS INTEGER    RANGE    10 DOWNTO       0;
  SUBTYPE cst_scl5 IS REAL       RANGE   0.0   TO      10.0;
  SUBTYPE cst_scl6 IS TIME       RANGE  0 fs   TO     10 ns;
  SUBTYPE cst_scl7 IS RESISTANCE RANGE  0 pf   TO  10000 pf;
  SUBTYPE cst_scl8 IS t_logic    RANGE    F0   TO        FX;
  
-- -----------------------------------------------------------------------------------------
--      Attribute Declarations
-- -----------------------------------------------------------------------------------------
--
  ATTRIBUTE atr_scl1 : st_scl1;
  ATTRIBUTE atr_scl2 : st_scl2;
  ATTRIBUTE atr_scl3 : st_scl3;
  ATTRIBUTE atr_scl4 : st_scl4;
  ATTRIBUTE atr_scl5 : st_scl5;
  ATTRIBUTE atr_scl6 : st_scl6;
  ATTRIBUTE atr_scl7 : st_scl7;
  ATTRIBUTE atr_scl8 : st_scl8;
  
  ATTRIBUTE atr_str1 : st_str1;
  ATTRIBUTE atr_str2 : st_str2;
  
  ATTRIBUTE cat_scl1 : cst_scl1;
  ATTRIBUTE cat_scl2 : cst_scl2;
  ATTRIBUTE cat_scl3 : cst_scl3;
  ATTRIBUTE cat_scl4 : cst_scl4;
  ATTRIBUTE cat_scl5 : cst_scl5;
  ATTRIBUTE cat_scl6 : cst_scl6;
  ATTRIBUTE cat_scl7 : cst_scl7;
  ATTRIBUTE cat_scl8 : cst_scl8;
-- =========================================================================================
--
--          Apply attributes to the package
--
  ATTRIBUTE atr_scl1 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS      TRUE;
  ATTRIBUTE atr_scl2 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS       '0';
  ATTRIBUTE atr_scl3 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS       'z';
  ATTRIBUTE atr_scl4 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS         0;
  ATTRIBUTE atr_scl5 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS      10.0;
  ATTRIBUTE atr_scl6 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS     10 ns;
  ATTRIBUTE atr_scl7 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS  10000 pf;
  ATTRIBUTE atr_scl8 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS        FX;
  
  ATTRIBUTE atr_str1 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS  "packit";
  ATTRIBUTE atr_str2 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS    "pack";
  
  ATTRIBUTE cat_scl1 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS      TRUE;
  ATTRIBUTE cat_scl2 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS       '0';
  ATTRIBUTE cat_scl3 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS       'z';
  ATTRIBUTE cat_scl4 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS         0;
  ATTRIBUTE cat_scl5 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS      10.0;
  ATTRIBUTE cat_scl6 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS     10 ns;
  ATTRIBUTE cat_scl7 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS  10000 pf;
  ATTRIBUTE cat_scl8 OF c06s03b00x00p08n01i00992pkg: PACKAGE IS        FX;
--
END;


use work.all;
use c06s03b00x00p08n01i00992pkg.all;
ENTITY c06s03b00x00p08n01i00992ent IS
END c06s03b00x00p08n01i00992ent;

ARCHITECTURE c06s03b00x00p08n01i00992arch OF c06s03b00x00p08n01i00992ent IS

BEGIN
  TESTING: PROCESS
  BEGIN
    ASSERT c06s03b00x00p08n01i00992pkg'atr_scl1 =      TRUE 
      REPORT "ERROR: Wrong value for 'atr_scl1" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'atr_scl2 =       '0' 
      REPORT "ERROR: Wrong value for 'atr_scl2" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'atr_scl3 =       'z' 
      REPORT "ERROR: Wrong value for 'atr_scl3" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'atr_scl4 =         0 
      REPORT "ERROR: Wrong value for 'atr_scl4" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'atr_scl5 =      10.0 
      REPORT "ERROR: Wrong value for 'atr_scl5" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'atr_scl6 =     10 ns 
      REPORT "ERROR: Wrong value for 'atr_scl6" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'atr_scl7 =  10000 pf 
      REPORT "ERROR: Wrong value for 'atr_scl7" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'atr_scl8 =        FX 
      REPORT "ERROR: Wrong value for 'atr_scl8" SEVERITY FAILURE;
    
    ASSERT c06s03b00x00p08n01i00992pkg'atr_str1 =  "packit" 
      REPORT "ERROR: Wrong value for 'atr_str1" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'atr_str2 =    "pack" 
      REPORT "ERROR: Wrong value for 'atr_str2" SEVERITY FAILURE;
    
    ASSERT c06s03b00x00p08n01i00992pkg'cat_scl1 =      TRUE 
      REPORT "ERROR: Wrong value for 'cat_scl1" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'cat_scl2 =       '0' 
      REPORT "ERROR: Wrong value for 'cat_scl2" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'cat_scl3 =       'z' 
      REPORT "ERROR: Wrong value for 'cat_scl3" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'cat_scl4 =         0 
      REPORT "ERROR: Wrong value for 'cat_scl4" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'cat_scl5 =      10.0 
      REPORT "ERROR: Wrong value for 'cat_scl5" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'cat_scl6 =     10 ns 
      REPORT "ERROR: Wrong value for 'cat_scl6" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'cat_scl7 =  10000 pf 
      REPORT "ERROR: Wrong value for 'cat_scl7" SEVERITY FAILURE;
    ASSERT c06s03b00x00p08n01i00992pkg'cat_scl8 =        FX 
      REPORT "ERROR: Wrong value for 'cat_scl8" SEVERITY FAILURE;

    assert NOT( c06s03b00x00p08n01i00992pkg'atr_scl1 =      TRUE 
                and c06s03b00x00p08n01i00992pkg'atr_scl2 =       '0' 
                and c06s03b00x00p08n01i00992pkg'atr_scl3 =       'z' 
                and c06s03b00x00p08n01i00992pkg'atr_scl4 =         0 
                and c06s03b00x00p08n01i00992pkg'atr_scl5 =      10.0 
                and c06s03b00x00p08n01i00992pkg'atr_scl6 =     10 ns 
                and c06s03b00x00p08n01i00992pkg'atr_scl7 =  10000 pf 
                and c06s03b00x00p08n01i00992pkg'atr_scl8 =        FX 
                and c06s03b00x00p08n01i00992pkg'atr_str1 =  "packit" 
                and c06s03b00x00p08n01i00992pkg'atr_str2 =    "pack" 
                and c06s03b00x00p08n01i00992pkg'cat_scl1 =      TRUE 
                and c06s03b00x00p08n01i00992pkg'cat_scl2 =       '0' 
                and c06s03b00x00p08n01i00992pkg'cat_scl3 =       'z' 
                and c06s03b00x00p08n01i00992pkg'cat_scl4 =         0 
                and c06s03b00x00p08n01i00992pkg'cat_scl5 =      10.0 
                and c06s03b00x00p08n01i00992pkg'cat_scl6 =     10 ns 
                and c06s03b00x00p08n01i00992pkg'cat_scl7 =  10000 pf 
                and c06s03b00x00p08n01i00992pkg'cat_scl8 =        FX) 
      report "***PASSED TEST: c06s03b00x00p08n01i00992"
      severity NOTE;
    assert (    c06s03b00x00p08n01i00992pkg'atr_scl1 =      TRUE 
                and c06s03b00x00p08n01i00992pkg'atr_scl2 =       '0' 
                and c06s03b00x00p08n01i00992pkg'atr_scl3 =       'z' 
                and c06s03b00x00p08n01i00992pkg'atr_scl4 =         0 
                and c06s03b00x00p08n01i00992pkg'atr_scl5 =      10.0 
                and c06s03b00x00p08n01i00992pkg'atr_scl6 =     10 ns 
                and c06s03b00x00p08n01i00992pkg'atr_scl7 =  10000 pf 
                and c06s03b00x00p08n01i00992pkg'atr_scl8 =        FX 
                and c06s03b00x00p08n01i00992pkg'atr_str1 =  "packit" 
                and c06s03b00x00p08n01i00992pkg'atr_str2 =    "pack" 
                and c06s03b00x00p08n01i00992pkg'cat_scl1 =      TRUE 
                and c06s03b00x00p08n01i00992pkg'cat_scl2 =       '0' 
                and c06s03b00x00p08n01i00992pkg'cat_scl3 =       'z' 
                and c06s03b00x00p08n01i00992pkg'cat_scl4 =         0 
                and c06s03b00x00p08n01i00992pkg'cat_scl5 =      10.0 
                and c06s03b00x00p08n01i00992pkg'cat_scl6 =     10 ns 
                and c06s03b00x00p08n01i00992pkg'cat_scl7 =  10000 pf 
                and c06s03b00x00p08n01i00992pkg'cat_scl8 =        FX) 
      report "***FAILED TEST: c06s03b00x00p08n01i00992 - Expanded name denotes a primary unit contained in design library test failed."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s03b00x00p08n01i00992arch;
