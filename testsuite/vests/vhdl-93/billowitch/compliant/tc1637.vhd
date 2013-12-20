
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
-- $Id: tc1637.vhd,v 1.2 2001-10-26 16:29:42 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c08s12b00x00p05n01i01637pkg is

  -- type declarations
  type ENUM is ( E1, E2, E3 );
  type DISTANCE is range 0 to 1E9
    units
      -- Base units.
      A;                    -- angstrom

      -- Metric lengths.
      nm       = 10 A;      -- nanometer
      um       = 1000 nm;   -- micrometer (or micron)
      mm       = 1000 um;   -- millimeter
      cm       = 10 mm;     -- centimeter

      -- English lengths.
      mil      = 254000 A;  -- mil
      inch     = 1000 mil;  -- inch
    end units;
  type ANARRAY is ARRAY( 0 to 1 ) of REAL;
  type ARECORD is
    RECORD
      Field1      : INTEGER;
      Field2      : BOOLEAN;
    end record;

  -- constant declarations
  CONSTANT CONSTI     : INTEGER  := 47;
  CONSTANT CONSTR     : REAL     := 47.0;
  CONSTANT CONSTE     : ENUM     := E1;
  CONSTANT CONSTD     : DISTANCE := 1 A;
  CONSTANT CONSTT     : TIME     := 1 hr;
  CONSTANT CONSTB     : BIT      := '1';
  CONSTANT CONSTS     : SEVERITY_LEVEL := WARNING;
  CONSTANT CONSTBO    : BOOLEAN  := FALSE;
  CONSTANT CONSTA     : ANARRAY  := ( 3.1415926, 4.0 );
  CONSTANT CONSTRE    : ARECORD  := ( Field1 => 2, Field2 => TRUE );
  
  -- function declarations.
  function funcI return INTEGER;
  function funcR return REAL;
  function funcE return ENUM;
  function funcD return DISTANCE;
  function funcT return TIME;
  function funcB return BIT;
  function funcS return SEVERITY_LEVEL;
  function funcBO return BOOLEAN;
  function funcA return ANARRAY;
  function funcRE return ARECORD;
  
end c08s12b00x00p05n01i01637pkg;

package body c08s12b00x00p05n01i01637pkg is
  
  function funcI return INTEGER is
  begin
    return( CONSTI );
  end;
  
  function funcR return REAL is
  begin
    return( CONSTR );
  end;
  
  function funcE return ENUM is
  begin
    return( CONSTE );
  end;
  
  function funcD return DISTANCE is
  begin
    return( CONSTD );
  end;
  
  function funcT return TIME is
  begin
    return( CONSTT );
  end;
  
  function funcB return BIT is
  begin
    return( CONSTB );
  end;
  
  function funcS return SEVERITY_LEVEL is
  begin
    return( CONSTS );
  end;
  
  function funcBO return BOOLEAN is
  begin
    return( CONSTBO );
  end;
  
  function funcA return ANARRAY is
  begin
    return( CONSTA );
  end;
  
  function funcRE return ARECORD is
  begin
    return( CONSTRE );
  end;
  
end c08s12b00x00p05n01i01637pkg;

use work.c08s12b00x00p05n01i01637pkg.all;
ENTITY c08s12b00x00p05n01i01637ent IS
END c08s12b00x00p05n01i01637ent;

ARCHITECTURE c08s12b00x00p05n01i01637arch OF c08s12b00x00p05n01i01637ent IS

BEGIN
  TESTING: PROCESS
    -- variable declarations.
    VARIABLE VARI     : INTEGER;
    VARIABLE VARR     : REAL;
    VARIABLE VARE     : ENUM;
    VARIABLE VARD     : DISTANCE;
    VARIABLE VART     : TIME;
    VARIABLE VARB     : BIT;
    VARIABLE VARS     : SEVERITY_LEVEL;
    VARIABLE VARBO    : BOOLEAN;
    VARIABLE VARA     : ANARRAY;
    VARIABLE VARRE    : ARECORD;
  BEGIN
    -- Call each function, verify that it returns the proper value.
    assert (funcI = CONSTI);
    assert (funcR = CONSTR);
    assert (funcE = CONSTE);
    assert (funcD = CONSTD);
    assert (funcT = CONSTT);
    assert (funcB = CONSTB);
    assert (funcS = CONSTS);
    assert (funcBO = CONSTBO);
    assert (funcA = CONSTA);
    assert (funcRE = CONSTRE);
    
    -- Assign function values to variables, make sure they're OK.
    VARI  := funcI;
    VARR  := funcR;
    VARE  := funcE;
    VARD  := funcD;
    VART  := funcT;
    VARB  := funcB;
    VARS  := funcS;
    VARBO := funcBO;
    VARA  := funcA;
    VARRE := funcRE;
    assert (VARI = CONSTI);
    assert (VARR = CONSTR);
    assert (VARE = CONSTE);
    assert (VARD = CONSTD);
    assert (VART = CONSTT);
    assert (VARB = CONSTB);
    assert (VARS = CONSTS);
    assert (VARBO = CONSTBO);
    assert (VARA = CONSTA);
    assert (VARRE = CONSTRE);

    assert NOT((funcI = CONSTI)   and
               (funcR = CONSTR)   and
               (funcE = CONSTE)   and
               (funcD = CONSTD)   and
               (funcT = CONSTT)   and
               (funcB = CONSTB)   and
               (funcS = CONSTS)   and
               (funcBO = CONSTBO)   and
               (funcA = CONSTA)   and
               (funcRE = CONSTRE)   and
               (VARI = CONSTI)   and
               (VARR = CONSTR)   and
               (VARE = CONSTE)   and
               (VARD = CONSTD)   and
               (VART = CONSTT)   and
               (VARB = CONSTB)   and
               (VARS = CONSTS)   and
               (VARBO = CONSTBO)   and
               (VARA = CONSTA)   and
               (VARRE = CONSTRE))   
      report "***PASSED TEST: c08s12b00x00p05n01i01637"
      severity NOTE;
    assert   ((funcI = CONSTI)   and
              (funcR = CONSTR)   and
              (funcE = CONSTE)   and
              (funcD = CONSTD)   and
              (funcT = CONSTT)   and
              (funcB = CONSTB)   and
              (funcS = CONSTS)   and
              (funcBO = CONSTBO)   and
              (funcA = CONSTA)   and
              (funcRE = CONSTRE)   and
              (VARI = CONSTI)   and
              (VARR = CONSTR)   and
              (VARE = CONSTE)   and
              (VARD = CONSTD)   and
              (VART = CONSTT)   and
              (VARB = CONSTB)   and
              (VARS = CONSTS)   and
              (VARBO = CONSTBO)   and
              (VARA = CONSTA)   and
              (VARRE = CONSTRE))   
      report "***FAILED TEST: c08s12b00x00p05n01i01637 - The value of the expression defines the result returned by the function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c08s12b00x00p05n01i01637arch;
