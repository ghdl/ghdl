----------------------------------------------------------------------------
--                                                                        --
--                   Copyright (c) 1993 by Mentor Graphics                --
--                                                                        --
--  This source file is proprietary information of Mentor Graphics,Inc.   --
--  It may be distributed in whole without restriction provided that      --   
--  this copyright statement is not removed from the file and that        --   
--  any derivative work contains this copyright notice.                   --
--                                                                        --
--  Package Name : std_logic_arith                                        --
--                                                                        --
--  Purpose : This package is to allow the synthesis of the 1164 package. --
--            This package add the capability of SIGNED/UNSIGNED math.    --
--                                                                        --
----------------------------------------------------------------------------

LIBRARY ieee ;

PACKAGE std_logic_arith IS
   

   USE ieee.std_logic_1164.ALL;

   TYPE SIGNED   IS ARRAY (Natural RANGE <>) OF STD_LOGIC ;
   TYPE UNSIGNED IS ARRAY (Natural RANGE <>) OF STD_LOGIC ;

   FUNCTION std_ulogic_wired_or ( input : std_ulogic_vector ) RETURN std_ulogic;
   FUNCTION std_ulogic_wired_and ( input : std_ulogic_vector ) RETURN std_ulogic;

   -------------------------------------------------------------------------------
   -- Note that all functions that take two vector arguments will
   -- handle unequal argument lengths
   -------------------------------------------------------------------------------

   -------------------------------------------------------------------    
   -- Conversion Functions 
   -------------------------------------------------------------------    
     
   --  Except for the to_integer and conv_integer functions for the
   --  signed argument all others assume the input vector to be of
   --  magnitude representation. The signed functions assume
   --  a 2's complement representation.
   FUNCTION to_integer ( arg1 : STD_ULOGIC_VECTOR; x : INTEGER := 0 ) RETURN INTEGER;
   FUNCTION to_integer ( arg1 : STD_LOGIC_VECTOR; x : INTEGER := 0 ) RETURN INTEGER;
   FUNCTION to_integer ( arg1 : STD_LOGIC; x : INTEGER := 0 ) RETURN NATURAL;
   FUNCTION to_integer ( arg1 : UNSIGNED; x : INTEGER := 0 ) RETURN NATURAL;
   FUNCTION to_integer ( arg1 : SIGNED; x : INTEGER := 0 ) RETURN INTEGER;

   FUNCTION conv_integer ( arg1 : STD_ULOGIC_VECTOR; x : INTEGER := 0 ) RETURN INTEGER;
   FUNCTION conv_integer ( arg1 : STD_LOGIC_VECTOR; x : INTEGER := 0 ) RETURN INTEGER;
   FUNCTION conv_integer ( arg1 : STD_LOGIC; x : INTEGER := 0 ) RETURN NATURAL;
   FUNCTION conv_integer ( arg1 : UNSIGNED; x : INTEGER := 0 ) RETURN NATURAL;
   FUNCTION conv_integer ( arg1 : SIGNED; x : INTEGER := 0 ) RETURN INTEGER;

   --  Following functions will return the natural argument in magnitude representation.
   FUNCTION to_stdlogic ( arg1 : BOOLEAN ) RETURN STD_LOGIC;
   FUNCTION to_stdlogicvector ( arg1 : INTEGER; size : NATURAL ) RETURN STD_LOGIC_VECTOR;
   FUNCTION to_stdulogicvector ( arg1 : INTEGER; size : NATURAL ) RETURN STD_ULOGIC_VECTOR;

   FUNCTION to_unsigned ( arg1 : NATURAL; size : NATURAL ) RETURN UNSIGNED;
   FUNCTION conv_unsigned ( arg1 : NATURAL; size : NATURAL ) RETURN UNSIGNED;

   --  The integer argument is returned in 2's complement representation.
   FUNCTION to_signed ( arg1 : INTEGER; size : NATURAL ) RETURN SIGNED;
   FUNCTION conv_signed ( arg1 : INTEGER; size : NATURAL ) RETURN SIGNED;

     
   -------------------------------------------------------------------------------
   -- sign/zero extend FUNCTIONs
   -------------------------------------------------------------------------------

   --  The zero_extend functions will perform zero padding to the input vector,
   --   returning a vector of length equal to size (the second argument). Note that
   --   if size is less than the length of the input argument an assertion will occur.
   FUNCTION zero_extend ( arg1 : STD_ULOGIC_VECTOR; size : NATURAL ) RETURN STD_ULOGIC_VECTOR;
   FUNCTION zero_extend ( arg1 : STD_LOGIC_VECTOR; size : NATURAL ) RETURN STD_LOGIC_VECTOR;
  FUNCTION zero_extend ( arg1 : STD_LOGIC; size : NATURAL ) RETURN STD_LOGIC_VECTOR;
   FUNCTION zero_extend ( arg1 : UNSIGNED; size : NATURAL ) RETURN UNSIGNED;
   FUNCTION sign_extend ( arg1 : SIGNED; size : NATURAL ) RETURN SIGNED;

     
   -------------------------------------------------------------------------------
   --  Arithmetic functions
   -------------------------------------------------------------------------------
     
   --  All arithmetic functions except multiplication will return a vector
   --     of size equal to the size of its largest argument. For multiplication,
   --   the resulting vector has a size equal to the sum of the size of its inputs.
   --   Note that arguments of unequal lengths are allowed.
   FUNCTION "+" ( arg1, arg2 : STD_LOGIC ) RETURN STD_LOGIC;
   FUNCTION "+" ( arg1, arg2 : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR;
   FUNCTION "+" ( arg1, arg2 : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR;
   FUNCTION "+" ( arg1, arg2 : UNSIGNED ) RETURN UNSIGNED ;
   FUNCTION "+" ( arg1, arg2 : SIGNED ) RETURN SIGNED ;

   FUNCTION "-" ( arg1, arg2 : STD_LOGIC ) RETURN STD_LOGIC;
   FUNCTION "-" ( arg1, arg2 : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR;
   FUNCTION "-" ( arg1, arg2 : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR;
   FUNCTION "-" ( arg1, arg2 : UNSIGNED ) RETURN UNSIGNED;
   FUNCTION "-" ( arg1, arg2 : SIGNED ) RETURN SIGNED;
     
   FUNCTION "+" ( arg1 : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR;
   FUNCTION "+" ( arg1 : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR;
   FUNCTION "+" ( arg1 : UNSIGNED ) RETURN UNSIGNED;
   FUNCTION "+" ( arg1 : SIGNED ) RETURN SIGNED;
   FUNCTION "-" ( arg1 : SIGNED ) RETURN SIGNED;
   
   FUNCTION "*" ( arg1, arg2 : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR;
   FUNCTION "*" ( arg1, arg2 : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR;
   FUNCTION "*" ( arg1, arg2 : UNSIGNED )         RETURN UNSIGNED ;
   FUNCTION "*" ( arg1, arg2 : SIGNED )           RETURN SIGNED ;

   FUNCTION "abs" ( arg1 : SIGNED) RETURN SIGNED;

   -- Vectorized Overloaded Arithmetic Operators, not supported for synthesis.
   -- The following operators are not supported for synthesis.
   FUNCTION "/"   ( l, r : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR;
   FUNCTION "/"   ( l, r : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR;
   FUNCTION "/"   ( l, r : UNSIGNED ) RETURN UNSIGNED;
   FUNCTION "/"   ( l, r : SIGNED ) RETURN SIGNED;
   FUNCTION "MOD"   ( l, r : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR;
   FUNCTION "MOD"   ( l, r : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR;
   FUNCTION "MOD"   ( l, r : UNSIGNED ) RETURN UNSIGNED;
   FUNCTION "REM"   ( l, r : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR;
   FUNCTION "REM"   ( l, r : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR;
   FUNCTION "REM"   ( l, r : UNSIGNED ) RETURN UNSIGNED;
   FUNCTION "**"   ( l, r : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR;
   FUNCTION "**"   ( l, r : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR;
   FUNCTION "**"   ( l, r : UNSIGNED ) RETURN UNSIGNED;

     
   -------------------------------------------------------------------------------
   --  Shift and rotate functions.
   -------------------------------------------------------------------------------

   --   Note that all the shift and rotate functions below will change to overloaded
   --   operators in the train1 release.
   FUNCTION "sla" (arg1:UNSIGNED          ; arg2:NATURAL)  RETURN UNSIGNED ;
   FUNCTION "sla" (arg1:SIGNED            ; arg2:NATURAL)  RETURN SIGNED ;
   FUNCTION "sla" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR ;
   FUNCTION "sla" (arg1:STD_LOGIC_VECTOR  ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR ;

   FUNCTION "sra" (arg1:UNSIGNED          ; arg2:NATURAL)  RETURN UNSIGNED ;
   FUNCTION "sra" (arg1:SIGNED            ; arg2:NATURAL)  RETURN SIGNED ;
   FUNCTION "sra" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR ;
   FUNCTION "sra" (arg1:STD_LOGIC_VECTOR  ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR ;
   
   FUNCTION "sll" (arg1:UNSIGNED          ; arg2:NATURAL)  RETURN UNSIGNED ;
   FUNCTION "sll" (arg1:SIGNED            ; arg2:NATURAL)  RETURN SIGNED ;
   FUNCTION "sll" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR ;
   FUNCTION "sll" (arg1:STD_LOGIC_VECTOR  ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR ;

   FUNCTION "srl" (arg1:UNSIGNED          ; arg2:NATURAL)  RETURN UNSIGNED ;
   FUNCTION "srl" (arg1:SIGNED            ; arg2:NATURAL)  RETURN SIGNED ;
   FUNCTION "srl" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR ;
   FUNCTION "srl" (arg1:STD_LOGIC_VECTOR  ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR ;

   FUNCTION "rol" (arg1:UNSIGNED          ; arg2:NATURAL)  RETURN UNSIGNED ;
   FUNCTION "rol" (arg1:SIGNED            ; arg2:NATURAL)  RETURN SIGNED ;
   FUNCTION "rol" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR ; 
   FUNCTION "rol" (arg1:STD_LOGIC_VECTOR  ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR ; 

   FUNCTION "ror" (arg1:UNSIGNED          ; arg2:NATURAL)  RETURN UNSIGNED ;
   FUNCTION "ror" (arg1:SIGNED            ; arg2:NATURAL)  RETURN SIGNED ;
   FUNCTION "ror" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR ; 
   FUNCTION "ror" (arg1:STD_LOGIC_VECTOR  ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR ; 

     
   -------------------------------------------------------------------------------
   --  Comparision functions and operators.
   -------------------------------------------------------------------------------

   --  For all comparision operators, the default operator for signed and unsigned
   --   types has been overloaded to perform logical comparisions. Note that for
   --   other types the default operator is not overloaded and the use will result
   --   in literal comparisions which is not supported for synthesis.
   --
   --  Unequal operator widths are supported for all the comparision functions.
   FUNCTION eq ( l, r : STD_LOGIC )         RETURN BOOLEAN;
   FUNCTION eq ( l, r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN;
   FUNCTION eq ( l, r : STD_LOGIC_VECTOR )  RETURN BOOLEAN;
   FUNCTION eq ( l, r : UNSIGNED )          RETURN BOOLEAN ;
   FUNCTION eq ( l, r : SIGNED )            RETURN BOOLEAN ;
   FUNCTION "=" ( l, r : UNSIGNED )         RETURN BOOLEAN ;
   FUNCTION "=" ( l, r : SIGNED )           RETURN BOOLEAN ;

   FUNCTION ne ( l, r : STD_LOGIC )         RETURN BOOLEAN;
   FUNCTION ne ( l, r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN;
   FUNCTION ne ( l, r : STD_LOGIC_VECTOR )  RETURN BOOLEAN;
   FUNCTION ne ( l, r : UNSIGNED )          RETURN BOOLEAN ;
   FUNCTION ne ( l, r : SIGNED )            RETURN BOOLEAN ;
   FUNCTION "/=" ( l, r : UNSIGNED )         RETURN BOOLEAN ;
   FUNCTION "/=" ( l, r : SIGNED )           RETURN BOOLEAN ;

   FUNCTION lt ( l, r : STD_LOGIC )         RETURN BOOLEAN;
   FUNCTION lt ( l, r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN;
   FUNCTION lt ( l, r : STD_LOGIC_VECTOR )  RETURN BOOLEAN;
   FUNCTION lt ( l, r : UNSIGNED )          RETURN BOOLEAN ;
   FUNCTION lt ( l, r : SIGNED )            RETURN BOOLEAN ;
   FUNCTION "<" ( l, r : UNSIGNED )         RETURN BOOLEAN ;
   FUNCTION "<" ( l, r : SIGNED )           RETURN BOOLEAN ;

   FUNCTION gt ( l, r : STD_LOGIC )         RETURN BOOLEAN;
   FUNCTION gt ( l, r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN;
   FUNCTION gt ( l, r : STD_LOGIC_VECTOR )  RETURN BOOLEAN;
   FUNCTION gt ( l, r : UNSIGNED )          RETURN BOOLEAN ;
   FUNCTION gt ( l, r : SIGNED )            RETURN BOOLEAN ;
   FUNCTION ">" ( l, r : UNSIGNED )         RETURN BOOLEAN ;
   FUNCTION ">" ( l, r : SIGNED )           RETURN BOOLEAN ;

   FUNCTION le ( l, r : STD_LOGIC )         RETURN BOOLEAN;
   FUNCTION le ( l, r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN;
   FUNCTION le ( l, r : STD_LOGIC_VECTOR )  RETURN BOOLEAN;
   FUNCTION le ( l, r : UNSIGNED )          RETURN BOOLEAN ;
   FUNCTION le ( l, r : SIGNED )            RETURN BOOLEAN ;
   FUNCTION "<=" ( l, r : UNSIGNED )         RETURN BOOLEAN ;
   FUNCTION "<=" ( l, r : SIGNED )           RETURN BOOLEAN ;

   FUNCTION ge ( l, r : STD_LOGIC )         RETURN BOOLEAN;
   FUNCTION ge ( l, r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN;
   FUNCTION ge ( l, r : STD_LOGIC_VECTOR )  RETURN BOOLEAN;
   FUNCTION ge ( l, r : UNSIGNED )          RETURN BOOLEAN ;
   FUNCTION ge ( l, r : SIGNED )            RETURN BOOLEAN ;
   FUNCTION ">=" ( l, r : UNSIGNED )         RETURN BOOLEAN ;
   FUNCTION ">=" ( l, r : SIGNED )           RETURN BOOLEAN ;

   -------------------------------------------------------------------------------
   --  Logical operators.
   -------------------------------------------------------------------------------

   --   allows operands of unequal lengths, return vector is
   --   equal to the size of the largest argument.

   FUNCTION "and"  (arg1, arg2:SIGNED)   RETURN SIGNED;
   FUNCTION "and"  (arg1, arg2:UNSIGNED) RETURN UNSIGNED;
   FUNCTION "nand" (arg1, arg2:SIGNED)   RETURN SIGNED;
   FUNCTION "nand" (arg1, arg2:UNSIGNED) RETURN UNSIGNED;
   FUNCTION "or"   (arg1, arg2:SIGNED)   RETURN SIGNED;
   FUNCTION "or"   (arg1, arg2:UNSIGNED) RETURN UNSIGNED;
   FUNCTION "nor"  (arg1, arg2:SIGNED)   RETURN SIGNED;
   FUNCTION "nor"  (arg1, arg2:UNSIGNED) RETURN UNSIGNED;
   FUNCTION "xor"  (arg1, arg2:SIGNED)   RETURN SIGNED;
   FUNCTION "xor"  (arg1, arg2:UNSIGNED) RETURN UNSIGNED;
   FUNCTION "not"  (arg1:SIGNED)         RETURN SIGNED;
   FUNCTION "not"  (arg1:UNSIGNED)       RETURN UNSIGNED;

   FUNCTION "xnor"  (arg1, arg2:STD_ULOGIC_VECTOR)  RETURN STD_ULOGIC_VECTOR;
   FUNCTION "xnor"  (arg1, arg2:STD_LOGIC_VECTOR)   RETURN STD_LOGIC_VECTOR;
   FUNCTION "xnor"  (arg1, arg2:SIGNED)   RETURN SIGNED;
   FUNCTION "xnor"  (arg1, arg2:UNSIGNED) RETURN UNSIGNED;
     
END std_logic_arith ;


