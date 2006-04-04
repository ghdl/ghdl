--------------------------------------------------------------- 
--
-- This source file may be used and distributed without restriction.
-- No declarations or definitions shall be added to this package.
-- This package cannot be sold or distributed for profit. 
--
--   ****************************************************************
--   *                                                              *
--   *                      W A R N I N G		 	    *
--   *								    *
--   *   This DRAFT version IS NOT endorsed or approved by IEEE     *
--   *							            *
--   ****************************************************************
--
-- Title:    PACKAGE BODY MATH_REAL
--
-- Library:  This package shall be compiled into a library 
--           symbolically named IEEE.
--
-- Purpose:  VHDL declarations for mathematical package MATH_REAL
--	     which contains common real constants, common real
--	     functions, and real trascendental functions.
--
-- Author:   IEEE VHDL Math Package Study Group 
--
-- Notes:
-- 	The package body shall be considered the formal definition of 
-- 	the semantics of this package. Tool developers may choose to implement 
-- 	the package body in the most efficient manner available to them.
--
--      Source code and algorithms for this package body comes from the 
--	following sources: 
--		IEEE VHDL Math Package Study Group participants,
--		U. of Mississippi, Mentor Graphics, Synopsys,
--		Viewlogic/Vantage, Communications of the ACM (June 1988, Vol
--		31, Number 6, pp. 747, Pierre L'Ecuyer, Efficient and Portable
--		Random Number Generators), Handbook of Mathematical Functions
--	        by Milton Abramowitz and Irene A. Stegun (Dover).
--
-- History:
-- 	Version 0.1	Jose A. Torres	4/23/93	First draft
-- 	Version 0.2	Jose A. Torres	5/28/93	Fixed potentially illegal code
--
-- GHDL history
--  2005-04-07  Initial version.
--  2005-12-23  I. Curtis : overhaul of log functions to bring in line
--                          with ieee standard
-------------------------------------------------------------
Library IEEE;

Package body MATH_REAL is
    --
    -- non-trascendental functions
    --
    function SIGN (X: real ) return real is
    	-- returns 1.0 if X > 0.0; 0.0 if X == 0.0; -1.0 if X < 0.0
    begin
        assert false severity failure;
    end SIGN; 

    function CEIL (X : real ) return real is
    begin
        assert false severity failure;
    end CEIL; 

    function FLOOR (X : real ) return real is
    begin
        assert false severity failure;
    end FLOOR;

    function ROUND (X : real ) return real is
    begin
        assert false severity failure;
    end ROUND;
    
    function FMAX (X, Y : real ) return real is
    begin
        assert false severity failure;
    end FMAX;

    function FMIN (X, Y : real ) return real is
    begin
        assert false severity failure;
    end FMIN;

    --
    -- Pseudo-random number generators
    --

    procedure UNIFORM(variable Seed1,Seed2:inout integer;variable X:out real) is
	-- returns a pseudo-random number with uniform distribution in the 
	-- interval (0.0, 1.0).
	-- Before the first call to UNIFORM, the seed values (Seed1, Seed2) must
	-- be initialized to values in the range [1, 2147483562] and 
	-- [1, 2147483398] respectively.  The seed values are modified after 
	-- each call to UNIFORM.
	-- This random number generator is portable for 32-bit computers, and
	-- it has period ~2.30584*(10**18) for each set of seed values.
	--
	-- For VHDL-1992, the seeds will be global variables, functions to 
	-- initialize their values (INIT_SEED) will be provided, and the UNIFORM
	-- procedure call will be modified accordingly.  
	
	variable z, k: integer;
	begin
        k := Seed1/53668;
        Seed1 := 40014 * (Seed1 - k * 53668) - k * 12211;
        
        if Seed1 < 0  then
            Seed1 := Seed1 + 2147483563;
        end if;


        k := Seed2/52774;
        Seed2 := 40692 * (Seed2 - k * 52774) - k * 3791;
        
        if Seed2 < 0  then
            Seed2 := Seed2 + 2147483399;
        end if;

        z := Seed1 - Seed2;
        if z < 1 then
            z := z + 2147483562;
        end if;

        X :=  REAL(Z)*4.656613e-10;
    end UNIFORM;


    function SRAND (seed: in integer ) return integer is
    begin
        assert false severity failure;
    end SRAND;

    function RAND return integer is
    begin
        assert false severity failure;
    end RAND;

    function GET_RAND_MAX  return integer is
        -- The value this function returns should be the same as
        -- RAND_MAX in /usr/include/stdlib.h
    begin
        assert false
            report "Be sure to update GET_RAND_MAX in mathpack.vhd"
            severity note;
        return 2147483647;  -- i386 linux
    end GET_RAND_MAX;

    --
    -- trascendental and trigonometric functions
    --
    function c_sqrt (x : real ) return real;
    attribute foreign of c_sqrt : function is "VHPIDIRECT sqrt";
    
    function c_sqrt (x : real ) return real is
    begin
        assert false severity failure;
    end c_sqrt;

    function SQRT (X : real ) return real is
    begin
        -- check validity of argument
        if ( X < 0.0 ) then
            assert false report "X < 0 in SQRT(X)" 
                severity ERROR;
            return (0.0);
        end if;
        return c_sqrt(X);
    end SQRT;

    function CBRT (X : real ) return real is
    begin
        assert false severity failure;
    end CBRT;

    function "**" (X : integer; Y : real) return real is
    	-- returns Y power of X ==>  X**Y;
    	-- error if X = 0 and Y <= 0.0
    	-- error if X < 0 and Y does not have an integer value
    begin
        -- check validity of argument
        if ( X = 0  ) and ( Y <= 0.0 ) then
            assert false report "X = 0 and Y <= 0.0 in X**Y" 
                severity ERROR;
            return (0.0);
        end if;

        if ( X < 0  ) and ( Y /= REAL(INTEGER(Y)) ) then
            assert false 
                report "X < 0 and Y \= integer in X**Y" 
                severity ERROR;
            return (0.0);
        end if;

        -- compute the result
        return EXP (Y * LOG (REAL(X)));
    end "**";

    function "**" (X : real; Y : real) return real is
    	-- returns Y power of X ==>  X**Y;
    	-- error if X = 0.0 and Y <= 0.0
    	-- error if X < 0.0 and Y does not have an integer value
    begin
        -- check validity of argument
        if ( X = 0.0  ) and ( Y <= 0.0 ) then
            assert false report "X = 0.0 and Y <= 0.0 in X**Y" 
                severity ERROR;
            return (0.0);
        end if;

        if ( X < 0.0  ) and ( Y /= REAL(INTEGER(Y)) ) then
            assert false report "X < 0.0 and Y \= integer in X**Y" 
                severity ERROR;
            return (0.0);
        end if;

        -- compute the result
        return EXP (Y * LOG (X));
    end "**";

    function EXP  (X : real ) return real is
    begin
        assert false severity failure;
    end EXP;

    function c_log (x : real ) return real;
    attribute foreign of c_log : function is "VHPIDIRECT log"; 

    function c_log (x : real ) return real is
    begin
        assert false severity failure;
    end c_log; 

    function LOG (X : real ) return real is
    	-- returns natural logarithm of X; X > 0
        --
        -- This function computes the exponential using the following series:
        --    log(x) = 2[ (x-1)/(x+1) + (((x-1)/(x+1))**3)/3.0 + ...] ; x > 0
        -- 
    begin
    	-- check validity of argument
    	if ( x <= 0.0 ) then
       	  assert false report "X <= 0 in LOG(X)" 
			severity ERROR;
            return(REAL'LOW);
    	end if;
    	return c_log(x); 
    end LOG;

    function LOG (X : in real; BASE: in real) return real is
      -- returns logarithm base BASE of X; X > 0
    begin
      -- check validity of argument
      if ( BASE <= 0.0 ) or ( x <= 0.0 ) then
        assert false report "BASE <= 0.0 or X <= 0.0 in LOG(BASE, X)" 
          severity ERROR;
        return(REAL'LOW);
      end if; 
      -- compute the value
      return (LOG(X)/LOG(BASE));
    end LOG;

    function LOG2 (X : in real) return real is
      -- returns logarithm BASE 2 of X; X > 0
    begin
      return LOG(X) / MATH_LOG_OF_2;
    end LOG2;

    function LOG10 (X : in real) return real is
      -- returns logarithm BASE 10 of X; X > 0
    begin
      return LOG(X) / MATH_LOG_OF_10;
    end LOG10;
    
    function  SIN (X : real ) return real is
    begin 
        assert false severity failure;
    end SIN;

   
    function COS (x : REAL) return REAL is 
    begin 
        assert false severity failure;
    end COS;
   
    function TAN (x : REAL) return REAL is 
    begin
        assert false severity failure;
    end TAN; 
    
    function c_asin (x : real ) return real;
    attribute foreign of c_asin : function is "VHPIDIRECT asin"; 

    function c_asin (x : real ) return real is
    begin
        assert false severity failure;
    end c_asin; 

    function ASIN (x : real ) return real is
        -- returns  -PI/2 < asin X < PI/2; | X | <= 1
    begin   
        if abs x > 1.0 then 
            assert false
                report "Out of range parameter passed to ASIN" 
                severity ERROR;
            return x;
        else
            return c_asin(x);
        end if; 
    end ASIN; 
   
    function c_acos (x : real ) return real;
    attribute foreign of c_acos : function is "VHPIDIRECT acos"; 

    function c_acos (x : real ) return real is
    begin
        assert false severity failure;
    end c_acos; 

    function ACOS (x : REAL) return REAL is
    	-- returns  0 < acos X < PI; | X | <= 1
    begin  
      if abs x > 1.0 then 
         assert false 
            report "Out of range parameter passed to ACOS" 
			severity ERROR; 
         return x;
      else
         return c_acos(x);
      end if;
    end ACOS; 
   
   function ATAN (x : REAL) return REAL is
    	-- returns  -PI/2 < atan X < PI/2
   begin
        assert false severity failure;
   end ATAN; 

    function c_atan2 (x : real; y : real) return real;
    attribute foreign of c_atan2 : function is "VHPIDIRECT atan2"; 

    function c_atan2 (x : real; y: real) return real is
    begin
        assert false severity failure;
    end c_atan2; 

    function ATAN2 (x : REAL; y : REAL) return REAL is 
        -- returns  atan (X/Y); -PI < atan2(X,Y) < PI; Y /= 0.0
    begin   
        if y = 0.0 and x = 0.0 then 
            assert false 
                report "atan2(0.0, 0.0) is undetermined, returned 0,0" 
                severity NOTE;
            return 0.0; 
        else
            return c_atan2(x,y);
        end if;     
    end ATAN2; 


    function SINH (X : real) return real is
    	-- hyperbolic sine; returns (e**X - e**(-X))/2
    begin
        assert false severity failure;
    end SINH;

    function  COSH (X : real) return real is
    	-- hyperbolic cosine; returns (e**X + e**(-X))/2
    begin
        assert false severity failure;
    end COSH;

    function  TANH (X : real) return real is
    	-- hyperbolic tangent; -- returns (e**X - e**(-X))/(e**X + e**(-X))
    begin
        assert false severity failure;
    end TANH;
    
    function ASINH (X : real) return real is
    	-- returns ln( X + sqrt( X**2 + 1))
    begin
        assert false severity failure;
    end ASINH;

    function c_acosh (x : real ) return real;
    attribute foreign of c_acosh : function is "VHPIDIRECT acosh"; 

    function c_acosh (x : real ) return real is
    begin
        assert false severity failure;
    end c_acosh;

    function ACOSH (X : real) return real is
    	-- returns ln( X + sqrt( X**2 - 1));   X >= 1
    begin
      	if abs x >= 1.0 then 
         	assert false report "Out of range parameter passed to ACOSH" 
			severity ERROR; 
         	return x;
      	end if; 
        return c_acosh(x);
    end ACOSH;

    function c_atanh (x : real ) return real;
    attribute foreign of c_atanh : function is "VHPIDIRECT atanh"; 

    function c_atanh (x : real ) return real is
    begin
        assert false severity failure;
    end c_atanh;

    function ATANH (X : real) return real is
    	-- returns (ln( (1 + X)/(1 - X)))/2 ; | X | < 1
    begin
      	if abs x < 1.0 then 
        	assert false report "Out of range parameter passed to ATANH" 
			severity ERROR; 
        	return x;
      	end if; 
        return c_atanh(x);
    end ATANH; 

end  MATH_REAL;
