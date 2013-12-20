
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
-- $Id: tc996.vhd,v 1.2 2001-10-26 16:30:04 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

-- too mangled up to fix for me

PACKAGE c06s03b00x00p08n01i00996pkg IS
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
  
-- ---------------------------------------------------------------------
--      Attribute Declarations
-- ---------------------------------------------------------------------
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
  
END;

USE WORK.c06s03b00x00p08n01i00996pkg.all;
ENTITY c06s03b00x00p08n01i00996ent IS
  GENERIC (
    p_scl1 : st_scl1;
    p_scl2 : st_scl2;
    p_scl3 : st_scl3;
    p_scl4 : st_scl4;
    p_scl5 : st_scl5;
    p_scl6 : st_scl6;
    p_scl7 : st_scl7;
    p_scl8 : st_scl8;
    p_str1 : st_str1;
    p_str2 : st_str2;
    labelid : STRING );
END c06s03b00x00p08n01i00996ent;

ARCHITECTURE c06s03b00x00p08n01i00996arch OF c06s03b00x00p08n01i00996ent IS
--  This entity behavior checks the values of attributes referenced at the configuration.
BEGIN
  PROCESS
  BEGIN
    ASSERT p_scl1 =      TRUE 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_scl1" SEVERITY FAILURE;
    ASSERT p_scl2 =       '0' 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_scl2" SEVERITY FAILURE;
    ASSERT p_scl3 =       'z' 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_scl3" SEVERITY FAILURE;
    ASSERT p_scl4 =         0 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_scl4" SEVERITY FAILURE;
    ASSERT p_scl5 =      10.0 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_scl5" SEVERITY FAILURE;
    ASSERT p_scl6 =     10 ns 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_scl6" SEVERITY FAILURE;
    ASSERT p_scl7 =  10000 pf 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_scl7" SEVERITY FAILURE;
    ASSERT p_scl8 =        FX 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_scl8" SEVERITY FAILURE;
    ASSERT p_str1 =  "signal" 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_str1" SEVERITY FAILURE;
    ASSERT p_str2 =    "XXXX" 
      REPORT "ERROR: Wrong value for " & labelid & "'atr_str2" SEVERITY FAILURE;
    
    assert NOT( p_scl1 =      TRUE 
                and p_scl2 =       '0' 
                and p_scl3 =       'z' 
                and p_scl4 =         0 
                and p_scl5 =      10.0 
                and p_scl6 =     10 ns 
                and p_scl7 =  10000 pf 
                and p_scl8 =        FX 
                and p_str1 =  "signal" 
                and p_str2 =    "XXXX") 
      report "***PASSED TEST: c06s03b00x00p08n01i00996"
      severity NOTE;
    assert    ( p_scl1 =      TRUE 
                and p_scl2 =       '0' 
                and p_scl3 =       'z' 
                and p_scl4 =         0 
                and p_scl5 =      10.0 
                and p_scl6 =     10 ns 
                and p_scl7 =  10000 pf 
                and p_scl8 =        FX 
                and p_str1 =  "signal" 
                and p_str2 =    "XXXX") 
      report "***FAILED TEST: c06s03b00x00p08n01i00996 - Expanded name denotes a primary unit contained in design library test failed."
      severity ERROR;
    wait;
  END PROCESS;
END;


USE WORK.c06s03b00x00p08n01i00996pkg.all;
ENTITY c06s03b00x00p08n01i00996ent_a IS
END c06s03b00x00p08n01i00996ent_a;

ARCHITECTURE c06s03b00x00p08n01i00996arch OF c06s03b00x00p08n01i00996ent IS
--
  COMPONENT c06s03b00x00p08n01i00996ent_a
  END COMPONENT;
  
BEGIN
  check  : c06s03b00x00p08n01i00996ent_a;
END c06s03b00x00p08n01i00996arch;

USE WORK.c06s03b00x00p08n01i00996pkg.all;
CONFIGURATION c06s03b00x00p08n01i00996cfg OF c06s03b00x00p08n01i00996ent IS
  ATTRIBUTE atr_scl1 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS      TRUE;
  ATTRIBUTE atr_scl2 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS       '0';
  ATTRIBUTE atr_scl3 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS       'z';
  ATTRIBUTE atr_scl4 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS         0;
  ATTRIBUTE atr_scl5 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS      10.0;
  ATTRIBUTE atr_scl6 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS     10 ns;
  ATTRIBUTE atr_scl7 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS  10000 pf;
  ATTRIBUTE atr_scl8 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS        FX;
  ATTRIBUTE atr_str1 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS  "signal";
  ATTRIBUTE atr_str2 OF c06s03b00x00p08n01i00996cfg: CONFIGURATION IS    "XXXX";
  
  FOR c06s03b00x00p08n01i00996arch
    FOR check : c06s03b00x00p08n01i00996ent_a  USE ENTITY WORK.c06s03b00x00p08n01i00996ent_a(c06s03b00x00p08n01i00996arch_a)
                                                 GENERIC MAP (
                                                   c06s03b00x00p08n01i00996cfg'atr_scl1,
                                                   c06s03b00x00p08n01i00996cfg'atr_scl2,
                                                   c06s03b00x00p08n01i00996cfg'atr_scl3,
                                                   c06s03b00x00p08n01i00996cfg'atr_scl4,
                                                   c06s03b00x00p08n01i00996cfg'atr_scl5,
                                                   c06s03b00x00p08n01i00996cfg'atr_scl6,
                                                   c06s03b00x00p08n01i00996cfg'atr_scl7,
                                                   c06s03b00x00p08n01i00996cfg'atr_scl8,
                                                   c06s03b00x00p08n01i00996cfg'atr_str1,
                                                   c06s03b00x00p08n01i00996cfg'atr_str2,
                                                   "work.c06s03b00x00p08n01i00996cfg" );
    END FOR;
  END FOR;
END c06s03b00x00p08n01i00996cfg;
