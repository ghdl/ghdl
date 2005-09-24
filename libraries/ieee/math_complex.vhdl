--------------------------------------------------------------- 
--
-- This source file may be used and distributed without restriction.
-- No declarations or definitions shall be included in this package.
-- This package cannot be sold or distributed for profit. 
--
--   ****************************************************************
--   *                                                              *
--   *                      W A R N I N G		 	    *
--   *								    *
--   *   This DRAFT version IS NOT endorsed or approved by IEEE     *
--   *								    *
--   ****************************************************************
--
-- Title:    PACKAGE MATH_COMPLEX
--
-- Purpose:  VHDL declarations for mathematical package MATH_COMPLEX
--	     which contains common complex constants and basic complex
--	     functions and operations.
--
-- Author:   IEEE VHDL Math Package Study Group 
--
-- Notes:     
--	The package body uses package IEEE.MATH_REAL
--
-- 	The package body shall be considered the formal definition of 
-- 	the semantics of this package. Tool developers may choose to implement 
-- 	the package body in the most efficient manner available to them.
--
-- History:
-- 	Version	0.1  (Strawman) Jose A. Torres	6/22/92
-- 	Version	0.2		Jose A. Torres	1/15/93
-- 	Version	0.3		Jose A. Torres	4/13/93
-- 	Version	0.4		Jose A. Torres 	4/19/93
-- 	Version	0.5		Jose A. Torres 	4/20/93
--	Version 0.6		Jose A. Torres  4/23/93  Added unary minus
--							 and CONJ for polar
--	Version 0.7		Jose A. Torres	5/28/93 Rev up for compatibility
--							with package body.
-------------------------------------------------------------
Library IEEE;

Package MATH_COMPLEX is


    type COMPLEX        is record RE, IM: real; end record;
    type COMPLEX_VECTOR is array (integer range <>) of COMPLEX;
    type COMPLEX_POLAR  is record MAG: real; ARG: real; end record;

    constant  CBASE_1: complex := COMPLEX'(1.0, 0.0);
    constant  CBASE_j: complex := COMPLEX'(0.0, 1.0);
    constant  CZERO: complex := COMPLEX'(0.0, 0.0);

    function CABS(Z: in complex ) return real;
    	-- returns absolute value (magnitude) of Z

    function CARG(Z: in complex ) return real;
    	-- returns argument (angle) in radians of a complex number

    function CMPLX(X: in real;  Y: in real:= 0.0 ) return complex;
    	-- returns complex number X + iY

    function "-" (Z: in complex ) return complex;
    	-- unary minus

    function "-" (Z: in complex_polar ) return complex_polar;
    	-- unary minus

    function CONJ (Z: in complex) return complex;
    	-- returns complex conjugate

    function CONJ (Z: in complex_polar) return complex_polar;
    	-- returns complex conjugate

    function CSQRT(Z: in complex ) return complex_vector;
    	-- returns square root of Z; 2 values

    function CEXP(Z: in complex ) return complex;
    	-- returns e**Z

    function COMPLEX_TO_POLAR(Z: in complex ) return complex_polar;
    	-- converts complex to complex_polar

    function POLAR_TO_COMPLEX(Z: in complex_polar ) return complex;
    	-- converts complex_polar to complex

    		
    -- arithmetic operators

    function "+" ( L: in complex;  R: in complex ) return complex;
    function "+" ( L: in complex_polar; R: in complex_polar) return complex;
    function "+" ( L: in complex_polar; R: in complex ) return complex;
    function "+" ( L: in complex;  R: in complex_polar) return complex;
    function "+" ( L: in real;     R: in complex ) return complex;
    function "+" ( L: in complex;  R: in real )    return complex;
    function "+" ( L: in real;  R: in complex_polar) return complex;
    function "+" ( L: in complex_polar;  R: in real) return complex;

    function "-" ( L: in complex;  R: in complex ) return complex;
    function "-" ( L: in complex_polar; R: in complex_polar) return complex;
    function "-" ( L: in complex_polar; R: in complex ) return complex;
    function "-" ( L: in complex;  R: in complex_polar) return complex;
    function "-" ( L: in real;     R: in complex ) return complex;
    function "-" ( L: in complex;  R: in real )    return complex;
    function "-" ( L: in real;  R: in complex_polar) return complex;
    function "-" ( L: in complex_polar;  R: in real) return complex;

    function "*" ( L: in complex;  R: in complex ) return complex;
    function "*" ( L: in complex_polar; R: in complex_polar) return complex;
    function "*" ( L: in complex_polar; R: in complex ) return complex;
    function "*" ( L: in complex;  R: in complex_polar) return complex;
    function "*" ( L: in real;     R: in complex ) return complex;
    function "*" ( L: in complex;  R: in real )    return complex;
    function "*" ( L: in real;  R: in complex_polar) return complex;
    function "*" ( L: in complex_polar;  R: in real) return complex;


    function "/" ( L: in complex;  R: in complex ) return complex;
    function "/" ( L: in complex_polar; R: in complex_polar) return complex;
    function "/" ( L: in complex_polar; R: in complex ) return complex;
    function "/" ( L: in complex;  R: in complex_polar) return complex;
    function "/" ( L: in real;     R: in complex ) return complex;
    function "/" ( L: in complex;  R: in real )    return complex;
    function "/" ( L: in real;  R: in complex_polar) return complex;
    function "/" ( L: in complex_polar;  R: in real) return complex;
end  MATH_COMPLEX;
