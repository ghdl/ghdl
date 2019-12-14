LIBRARY ieee;
-- LIBRARY arithmetic;

PACKAGE BODY std_logic_arith IS

    USE ieee.std_logic_1164.ALL;
   -- USE arithmetic.utils.all; 
    
    -------------------------------------------------------------------    
    -- Local Types
    -------------------------------------------------------------------    
    TYPE stdlogic_1d IS ARRAY (std_ulogic) OF std_ulogic;
    TYPE stdlogic_table IS ARRAY(std_ulogic, std_ulogic) OF std_ulogic;
    TYPE stdlogic_boolean_table IS ARRAY(std_ulogic, std_ulogic) OF BOOLEAN;

    --------------------------------------------------------------------
    --------------------------------------------------------------------
    -- FUNCTIONS DEFINED FOR SYNTHESIS
    --------------------------------------------------------------------
    --------------------------------------------------------------------

    FUNCTION std_ulogic_wired_or ( input : std_ulogic_vector ) RETURN std_ulogic IS
        VARIABLE result : std_ulogic := '-';  -- weakest state default
        CONSTANT resolution_table : stdlogic_table := (
	    --  ---------------------------------------------------------
            --  |  U    X    0    1    Z    W    L    H    D 	    |   |  
	    --  ---------------------------------------------------------
                ( 'X', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' ), -- | U |
                ( 'X', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' ), -- | X |
                ( 'X', 'X', '0', '1', '0', 'X', '0', '1', '0' ), -- | 0 |
                ( '1', '1', '1', '1', '1', '1', '1', '1', '1' ), -- | 1 |
                ( 'X', 'X', '0', '1', 'Z', 'X', '0', '1', 'Z' ), -- | Z |
                ( 'X', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' ), -- | W |
                ( 'X', 'X', '0', '1', '0', 'X', '0', '1', '0' ), -- | L |
                ( '1', '1', '1', '1', '1', '1', '1', '1', '1' ), -- | H |
                ( 'X', 'X', '0', '1', 'Z', 'X', '0', '1', 'Z' )  -- | D |
            );
        
    BEGIN
         -- Iterate through all inputs
         FOR i IN input'range LOOP
             result := resolution_table(result, input(i));
         END LOOP;
         -- Return the resultant value 
         RETURN result;
    END std_ulogic_wired_or;

    FUNCTION std_ulogic_wired_and ( input : std_ulogic_vector ) RETURN std_ulogic IS
        VARIABLE result : std_ulogic := '-';  -- weakest state default
        CONSTANT resolution_table : stdlogic_table := (
	    --  ---------------------------------------------------------
            --  |  U    X    0    1    Z    W    L    H    D 	    |   |  
	    --  ---------------------------------------------------------
                ( 'X', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' ), -- | U |
                ( 'X', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' ), -- | X |
                ( '0', '0', '0', '0', '0', '0', '0', '0', '0' ), -- | 0 |
                ( 'X', 'X', '0', '1', '1', 'X', '0', '1', '1' ), -- | 1 |
                ( 'X', 'X', '0', '1', 'Z', 'X', '0', '1', 'Z' ), -- | Z |
                ( 'X', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' ), -- | W |
                ( '0', '0', '0', '0', '0', '0', '0', '0', '0' ), -- | L |
                ( 'X', 'X', '0', '1', '1', 'X', '0', '1', '1' ), -- | H |
                ( 'X', 'X', '0', '1', 'Z', 'X', '0', '1', 'Z' )  -- | D |
            );
        
    BEGIN
         -- Iterate through all inputs
         FOR i IN input'range LOOP
             result := resolution_table(result, input(i));
         END LOOP;
         -- Return the resultant value 
         RETURN result;
    END std_ulogic_wired_and;

--
-- MGC base level functions
--
--
-- Convert Base Type to Integer
--
      FUNCTION to_integer (arg1 : STD_ULOGIC_VECTOR; x : INTEGER := 0 ) RETURN INTEGER IS
         VARIABLE tmp : SIGNED( arg1'length - 1 DOWNTO 0 ) := (OTHERS => '0');
         VARIABLE       result   : INTEGER;
      BEGIN
         tmp := SIGNED(arg1);
         result := TO_INTEGER( tmp, x );
         RETURN (result);
      END to_integer;

      FUNCTION to_integer (arg1 : STD_LOGIC_VECTOR; x : INTEGER := 0 ) RETURN INTEGER IS
         VARIABLE tmp : SIGNED( arg1'length - 1 DOWNTO 0 ) := (OTHERS => '0');
         VARIABLE       result   : INTEGER;
      BEGIN
         tmp := SIGNED(arg1);
         result := TO_INTEGER( tmp, x );
         RETURN (result);
      END to_integer;

      FUNCTION to_integer (arg1 : UNSIGNED; x : INTEGER := 0 ) RETURN NATURAL IS
         VARIABLE tmp : SIGNED( arg1'length DOWNTO 0 ) := (OTHERS => '0');
         VARIABLE       result   : NATURAL;
      BEGIN
         tmp := '0' & SIGNED(arg1);
         result := TO_INTEGER( tmp, x );
         RETURN (result);
      END to_integer;

      FUNCTION TO_INTEGER (arg1 : SIGNED; x : INTEGER := 0 ) RETURN INTEGER IS
         VARIABLE return_int,x_tmp : INTEGER := 0;
      BEGIN
         ASSERT arg1'length > 0
            REPORT "NULL vector, returning 0"
            SEVERITY NOTE;
         assert arg1'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
         ASSERT arg1'length <= 32     -- implementation dependent limit
            REPORT "vector too large, conversion may cause overflow"
            SEVERITY WARNING;
         IF x /= 0 THEN 
            x_tmp := 1;
         END IF;
         IF arg1(arg1'left) = '0' OR arg1(arg1'left) = 'L' OR  -- positive value
            ( x_tmp = 0 AND arg1(arg1'left) /= '1' AND arg1(arg1'left) /= 'H') THEN 
            FOR i IN arg1'range LOOP
               return_int := return_int * 2;
               CASE arg1(i) IS
                  WHEN '0'|'L' =>     NULL;
                  WHEN '1'|'H' =>     return_int := return_int + 1;
                  WHEN OTHERS  =>     return_int := return_int + x_tmp;
               END CASE;
            END LOOP;
         ELSE                                 -- negative value
           IF (x_tmp = 0) THEN
             x_tmp := 1;
           ELSE
             x_tmp := 0;
           END IF;
            FOR i IN arg1'range LOOP
               return_int := return_int * 2;
               CASE arg1(i) IS
                  WHEN '0'|'L' =>     return_int := return_int + 1;
                  WHEN '1'|'H' =>     NULL;
                  WHEN OTHERS  =>     return_int := return_int + x_tmp;
               END CASE;
            END LOOP;
            return_int := (-return_int) - 1;
         END IF;
         RETURN return_int;
      END TO_INTEGER;

      FUNCTION to_integer (arg1:STD_LOGIC; x : INTEGER := 0 )        RETURN NATURAL IS
      BEGIN
          IF(arg1 = '0' OR arg1 = 'L' OR (x = 0 AND arg1 /= '1' AND arg1 /= 'H')) THEN
            RETURN(0);
          ELSE
            RETURN(1) ;
          END IF ;
      END ;

      FUNCTION conv_integer (arg1 : STD_ULOGIC_VECTOR; x : INTEGER := 0 ) RETURN INTEGER IS
         VARIABLE tmp : SIGNED( arg1'length - 1 DOWNTO 0 ) := (OTHERS => '0');
         VARIABLE       result   : INTEGER;
      BEGIN
         tmp := SIGNED(arg1);
         result := TO_INTEGER( tmp, x );
         RETURN (result);
      END ;

      FUNCTION conv_integer (arg1 : STD_LOGIC_VECTOR; x : INTEGER := 0 ) RETURN INTEGER IS
         VARIABLE tmp : SIGNED( arg1'length -1 DOWNTO 0 ) := (OTHERS => '0');
         VARIABLE       result   : INTEGER;
      BEGIN
         tmp := SIGNED(arg1);
         result := TO_INTEGER( tmp, x );
         RETURN (result);
      END ;

      FUNCTION conv_integer (arg1 : UNSIGNED; x : INTEGER := 0 ) RETURN NATURAL IS
         VARIABLE tmp : SIGNED( arg1'length DOWNTO 0 ) := (OTHERS => '0');
         VARIABLE       result   : NATURAL;
      BEGIN
         tmp := '0' & SIGNED(arg1);
         result := TO_INTEGER( tmp, x );
         RETURN (result);
      END ;

      FUNCTION conv_INTEGER (arg1 : SIGNED; x : INTEGER := 0 ) RETURN INTEGER IS
         VARIABLE return_int,x_tmp : INTEGER := 0;
      BEGIN
         ASSERT arg1'length > 0
            REPORT "NULL vector, returning 0"
            SEVERITY NOTE;
         assert arg1'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
         ASSERT arg1'length <= 32     -- implementation dependent limit
            REPORT "vector too large, conversion may cause overflow"
            SEVERITY WARNING;
         IF x /= 0 THEN 
            x_tmp := 1;
         END IF;
         IF arg1(arg1'left) = '0' OR arg1(arg1'left) = 'L' OR  -- positive value
            ( x_tmp = 0 AND arg1(arg1'left) /= '1' AND arg1(arg1'left) /= 'H') THEN 
            FOR i IN arg1'range LOOP
               return_int := return_int * 2;
               CASE arg1(i) IS
                  WHEN '0'|'L' =>     NULL;
                  WHEN '1'|'H' =>     return_int := return_int + 1;
                  WHEN OTHERS  =>     return_int := return_int + x_tmp;
               END CASE;
            END LOOP;
         ELSE                                 -- negative value
           IF (x_tmp = 0) THEN
             x_tmp := 1;
           ELSE
             x_tmp := 0;
           END IF;
            FOR i IN arg1'range LOOP
               return_int := return_int * 2;
               CASE arg1(i) IS
                  WHEN '0'|'L' =>     return_int := return_int + 1;
                  WHEN '1'|'H' =>     NULL;
                  WHEN OTHERS  =>     return_int := return_int + x_tmp;
               END CASE;
            END LOOP;
            return_int := (-return_int) - 1;
         END IF;
         RETURN return_int;
      END ;

      FUNCTION conv_integer (arg1:STD_LOGIC; x : INTEGER := 0 )        RETURN NATURAL IS
      BEGIN
          IF(arg1 = '0' OR arg1 = 'L' OR (x = 0 AND arg1 /= '1' AND arg1 /= 'H')) THEN
            RETURN(0);
          ELSE
            RETURN(1) ;
          END IF ;
      END ;

--
-- Convert Base Type to STD_LOGIC
--
   
  FUNCTION to_stdlogic (arg1:BOOLEAN)  RETURN STD_LOGIC IS
      BEGIN
      IF(arg1) THEN
        RETURN('1') ;
      ELSE
        RETURN('0') ;
      END IF ;
  END ;

--
-- Convert Base Type to STD_LOGIC_VECTOR
--
      FUNCTION To_StdlogicVector (arg1 : integer; size : NATURAL) RETURN std_logic_vector IS
         VARIABLE vector : std_logic_vector(0 TO size-1);
         VARIABLE tmp_int : integer := arg1;
         VARIABLE carry   : std_logic := '1';   -- setup to add 1 if needed
         VARIABLE carry2  : std_logic;
      BEGIN    
         FOR i IN size-1 DOWNTO 0 LOOP
             IF tmp_int MOD 2 = 1 THEN
                vector(i) := '1';
             ELSE
                vector(i) := '0';
             END IF;
             tmp_int := tmp_int / 2;
         END LOOP; 

         IF arg1 < 0 THEN
            FOR i IN size-1 DOWNTO 0 LOOP
          	carry2    := (NOT vector(i)) AND carry;
          	vector(i) := (NOT vector(i)) XOR carry;
                carry     := carry2;
            END LOOP; 
         END IF;
         RETURN vector;
      END To_StdlogicVector; 

      FUNCTION To_StdUlogicVector (arg1 : integer; size : NATURAL) RETURN std_ulogic_vector IS
         VARIABLE vector : std_ulogic_vector(0 TO size-1);
         VARIABLE tmp_int : integer := arg1;
         VARIABLE carry   : std_ulogic := '1';   -- setup to add 1 if needed
         VARIABLE carry2  : std_ulogic;
      BEGIN    
         FOR i IN size-1 DOWNTO 0 LOOP
             IF tmp_int MOD 2 = 1 THEN
                vector(i) := '1';
             ELSE
                vector(i) := '0';
             END IF;
             tmp_int := tmp_int / 2;
         END LOOP; 

         IF arg1 < 0 THEN
            FOR i IN size-1 DOWNTO 0 LOOP
          	carry2    := (NOT vector(i)) AND carry;
          	vector(i) := (NOT vector(i)) XOR carry;
                carry     := carry2;
            END LOOP; 
         END IF;
         RETURN vector;
      END To_StdUlogicVector; 
      

--
-- Convert Base Type to UNSIGNED
--
 
  FUNCTION to_unsigned (arg1:NATURAL          ; size:NATURAL) RETURN UNSIGNED IS
         VARIABLE vector : UNSIGNED(0 TO size-1) := (OTHERS => '0');
         VARIABLE tmp_int : INTEGER := arg1;
      BEGIN    
         FOR i IN size-1 DOWNTO 0 LOOP
             IF tmp_int MOD 2 = 1 THEN
                vector(i) := '1';
             ELSE
                vector(i) := '0';
             END IF;
             tmp_int := tmp_int / 2;
         END LOOP; 

         RETURN vector;
  END ;

  FUNCTION conv_unsigned (arg1:NATURAL          ; size:NATURAL) RETURN UNSIGNED IS
         VARIABLE vector : UNSIGNED(0 TO size-1) := (OTHERS => '0');
         VARIABLE tmp_int : INTEGER := arg1;
      BEGIN    
         FOR i IN size-1 DOWNTO 0 LOOP
             IF tmp_int MOD 2 = 1 THEN
                vector(i) := '1';
             ELSE
                vector(i) := '0';
             END IF;
             tmp_int := tmp_int / 2;
         END LOOP; 

         RETURN vector;
  END ;

--
-- Convert Base Type to SIGNED
--

  FUNCTION to_signed (arg1:INTEGER          ; size : NATURAL) RETURN SIGNED IS
         VARIABLE vector : SIGNED(0 TO size-1) := (OTHERS => '0');
         VARIABLE tmp_int : INTEGER := arg1;
         VARIABLE carry   : STD_LOGIC := '1';   -- setup to add 1 if needed
         VARIABLE carry2  : STD_LOGIC := '0';
      BEGIN    
         FOR i IN size-1 DOWNTO 0 LOOP
             IF tmp_int MOD 2 = 1 THEN
                vector(i) := '1';
             ELSE
                vector(i) := '0';
             END IF;
             tmp_int := tmp_int / 2;
         END LOOP; 

         IF arg1 < 0 THEN
            FOR i IN size-1 DOWNTO 0 LOOP
          	carry2    := (NOT vector(i)) AND carry;
          	vector(i) := (NOT vector(i)) XOR carry;
                carry     := carry2;
            END LOOP; 
         END IF;
         RETURN vector;
      END ; 

  FUNCTION conv_signed (arg1:INTEGER          ; size : NATURAL) RETURN SIGNED IS
         VARIABLE vector : SIGNED(0 TO size-1) := (OTHERS => '0');
         VARIABLE tmp_int : INTEGER := arg1;
         VARIABLE carry   : STD_LOGIC := '1';   -- setup to add 1 if needed
         VARIABLE carry2  : STD_LOGIC := '0';
      BEGIN    
         FOR i IN size-1 DOWNTO 0 LOOP
             IF tmp_int MOD 2 = 1 THEN
                vector(i) := '1';
             ELSE
                vector(i) := '0';
             END IF;
             tmp_int := tmp_int / 2;
         END LOOP; 

         IF arg1 < 0 THEN
            FOR i IN size-1 DOWNTO 0 LOOP
          	carry2    := (NOT vector(i)) AND carry;
          	vector(i) := (NOT vector(i)) XOR carry;
                carry     := carry2;
            END LOOP; 
         END IF;
         RETURN vector;
      END ; 

  -- sign/zero extend functions
  --

   FUNCTION zero_extend ( arg1 : STD_ULOGIC_VECTOR; size : NATURAL ) RETURN STD_ULOGIC_VECTOR
     IS 
     VARIABLE answer : STD_ULOGIC_VECTOR(size-1 DOWNTO 0) := (OTHERS => '0') ;
    BEGIN
       ASSERT arg1'length <= size
         REPORT "Vector is already larger then size."
         SEVERITY WARNING ;
      answer := (OTHERS => '0') ;
      answer(arg1'length-1 DOWNTO 0) := arg1;
      RETURN(answer) ;     
  END ;

   FUNCTION zero_extend ( arg1 : STD_LOGIC_VECTOR; size : NATURAL ) RETURN STD_LOGIC_VECTOR
     IS 
     VARIABLE answer : STD_LOGIC_VECTOR(size-1 DOWNTO 0) := (OTHERS => '0') ;
    BEGIN
       ASSERT arg1'length <= size
         REPORT "Vector is already larger then size."
         SEVERITY WARNING ;
      answer := (OTHERS => '0') ;
      answer(arg1'length-1 DOWNTO 0) := arg1;
      RETURN(answer) ;     
  END ;

   FUNCTION zero_extend ( arg1 : STD_LOGIC; size : NATURAL ) RETURN STD_LOGIC_VECTOR
     IS
    VARIABLE answer : STD_LOGIC_VECTOR(size-1 DOWNTO 0) := (OTHERS => '0') ;
    BEGIN
      answer := (OTHERS => '0') ;
      answer(0) := arg1;
      RETURN(answer) ;     
  END ;

  FUNCTION zero_extend ( arg1 : UNSIGNED; size : NATURAL ) RETURN UNSIGNED IS 
    VARIABLE answer : UNSIGNED(size-1 DOWNTO 0) := (OTHERS => '0') ;
    BEGIN
       ASSERT arg1'length <= size
         REPORT "Vector is already larger then size."
         SEVERITY WARNING ;
      answer := (OTHERS => '0') ;
      answer(arg1'length - 1 DOWNTO 0) := arg1;
      RETURN(answer) ;     
  END ;

  FUNCTION sign_extend ( arg1 : SIGNED; size : NATURAL ) RETURN SIGNED IS
    VARIABLE answer : SIGNED(size-1 DOWNTO 0) := (OTHERS => '0') ;
    BEGIN
       ASSERT arg1'length <= size
         REPORT "Vector is already larger then size."
         SEVERITY WARNING ;
      answer := (OTHERS => arg1(arg1'left)) ;
      answer(arg1'length - 1 DOWNTO 0) := arg1;
      RETURN(answer) ;     
  END ;



    -- Some useful generic functions
    
    --//// Zero Extend ////
    --
    -- Function zxt
    --
    FUNCTION zxt( q : STD_ULOGIC_VECTOR; i : INTEGER ) RETURN STD_ULOGIC_VECTOR IS
        VARIABLE qs : STD_ULOGIC_VECTOR (1 TO i);
        VARIABLE qt : STD_ULOGIC_VECTOR (1 TO q'length);
    BEGIN
        qt := q;
        IF i < q'length THEN
            qs := qt( (q'length-i+1) TO qt'right);
        ELSIF i > q'length THEN
            qs := (OTHERS=>'0');
            qs := qs(1 TO (i-q'length)) & qt;
        ELSE
            qs := qt;
        END IF;
        RETURN qs;
    END;
    
    --//// Zero Extend ////
    --
    -- Function zxt
    --
    FUNCTION zxt( q : STD_LOGIC_VECTOR; i : INTEGER ) RETURN STD_LOGIC_VECTOR IS
        VARIABLE qs : STD_LOGIC_VECTOR (1 TO i);
        VARIABLE qt : STD_LOGIC_VECTOR (1 TO q'length);
    BEGIN
        qt := q;
        IF i < q'length THEN
            qs := qt( (q'length-i+1) TO qt'right);
        ELSIF i > q'length THEN
            qs := (OTHERS=>'0');
            qs := qs(1 TO (i-q'length)) & qt;
        ELSE
            qs := qt;
        END IF;
        RETURN qs;
    END;
    
    --//// Zero Extend ////
    --
    -- Function zxt
    --
    FUNCTION zxt( q : UNSIGNED; i : INTEGER ) RETURN UNSIGNED IS
        VARIABLE qs : UNSIGNED (1 TO i);
        VARIABLE qt : UNSIGNED (1 TO q'length);
    BEGIN
        qt := q;
        IF i < q'length THEN
            qs := qt( (q'length-i+1) TO qt'right);
        ELSIF i > q'length THEN
            qs := (OTHERS=>'0');
            qs := qs(1 TO (i-q'length)) & qt;
        ELSE
            qs := qt;
        END IF;
        RETURN qs;
    END;
    
--------------------------------------
-- Synthesizable addition Functions --
--------------------------------------

    FUNCTION "+"  ( arg1, arg2 : STD_LOGIC ) RETURN STD_LOGIC IS
	-- truth table for "xor" function
	CONSTANT xor_table : stdlogic_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |  
	--      ----------------------------------------------------
                ( 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U' ),  -- | U |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | X |
	        ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | 0 |
	        ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', 'X' ),  -- | 1 |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | Z |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | W |
	        ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | L |
	        ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', 'X' ),  -- | H |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' )   -- | D |
    );
    BEGIN
        RETURN xor_table( arg1, arg2 );
    END "+";

  function maximum (arg1, arg2: integer) return integer is
  begin
   if arg1 > arg2 then
     return arg1;
   else
     return arg2;
   end if;
  end;

  FUNCTION "+" (arg1, arg2 :STD_ULOGIC_VECTOR) RETURN STD_ULOGIC_VECTOR IS
        CONSTANT ml     : INTEGER := maximum(arg1'length,arg2'length);
        VARIABLE lt     : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE rt     : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE res    : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE carry  : STD_ULOGIC := '0';
        VARIABLE a,b,s1 : STD_ULOGIC;
    BEGIN       
        lt := zxt( arg1, ml );
        rt := zxt( arg2, ml );
      
        FOR i IN res'reverse_range LOOP
          a := lt(i);
          b := rt(i);
          s1 := a + b;
          res(i) := s1 + carry;
          carry := (a AND b) OR (s1 AND carry);
        END LOOP;           
        RETURN res;
      END;
      
  FUNCTION "+" (arg1, arg2 :STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
        CONSTANT ml     : INTEGER := maximum(arg1'length,arg2'length);
        VARIABLE lt     : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE rt     : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE res    : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE carry  : STD_LOGIC := '0';
        VARIABLE a,b,s1 : STD_LOGIC;
    BEGIN       
        lt := zxt( arg1, ml );
        rt := zxt( arg2, ml );
      
        FOR i IN res'reverse_range LOOP
          a := lt(i);
          b := rt(i);
          s1 := a + b;
          res(i) := s1 + carry;
          carry := (a AND b) OR (s1 AND carry);
        END LOOP;           
        RETURN res;
      END;
      
  FUNCTION "+" (arg1, arg2:UNSIGNED)         RETURN UNSIGNED IS
        CONSTANT ml     : INTEGER := maximum(arg1'length,arg2'length);
        VARIABLE lt     : UNSIGNED(1 TO ml);
        VARIABLE rt     : UNSIGNED(1 TO ml);
        VARIABLE res    : UNSIGNED(1 TO ml);
        VARIABLE carry  : STD_LOGIC := '0';
        VARIABLE a,b,s1 : STD_LOGIC;
    BEGIN       
        lt := zxt( arg1, ml );
        rt := zxt( arg2, ml );
      
        FOR i IN res'reverse_range LOOP
          a := lt(i);
          b := rt(i);
          s1 := a + b;
          res(i) := s1 + carry;
          carry := (a AND b) OR (s1 AND carry);
        END LOOP;           
        RETURN res;
      END;
      
  FUNCTION "+" (arg1, arg2:SIGNED)           RETURN SIGNED IS
    CONSTANT len : INTEGER := maximum(arg1'length,arg2'length) ;
    VARIABLE a,b : UNSIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
    VARIABLE answer : SIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
      BEGIN
         assert arg1'length > 1 AND arg2'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      a := (OTHERS => arg1(arg1'left)) ;
      a(arg1'length - 1 DOWNTO 0) := UNSIGNED(arg1);
      b := (OTHERS => arg2(arg2'left)) ;
      b(arg2'length - 1 DOWNTO 0) := UNSIGNED(arg2);
      answer := SIGNED(a + b);
      RETURN (answer);
  END ;

-----------------------------------------
-- Synthesizable subtraction Functions --
-----------------------------------------

    FUNCTION "-"  ( arg1, arg2 : std_logic ) RETURN std_logic IS
	-- truth table for "xor" function
	CONSTANT xor_table : stdlogic_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |  
	--      ----------------------------------------------------
                ( 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U' ),  -- | U |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | X |
	        ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | 0 |
	        ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', 'X' ),  -- | 1 |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | Z |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | W |
	        ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | L |
	        ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', 'X' ),  -- | H |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' )   -- | D |
    );
    BEGIN
        RETURN xor_table( arg1, arg2 );
    END "-";

  FUNCTION "-" (arg1, arg2:STD_ULOGIC_VECTOR) RETURN STD_ULOGIC_VECTOR IS
        CONSTANT ml     : INTEGER := maximum(arg1'length,arg2'length);
        VARIABLE lt     : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE rt     : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE res    : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE borrow : STD_ULOGIC := '1';
        VARIABLE a,b,s1 : STD_ULOGIC;
      BEGIN       
        lt := zxt( arg1, ml );
        rt := zxt( arg2, ml );
      
        FOR i IN res'reverse_range LOOP
          a := lt(i);
          b := NOT rt(i);
          s1 := a + b;
          res(i) := s1 + borrow;
          borrow := (a AND b) OR (s1 AND borrow);
        END LOOP;           
        RETURN res;                
      END "-";  

  FUNCTION "-" (arg1, arg2:STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
        CONSTANT ml     : INTEGER := maximum(arg1'length,arg2'length);
        VARIABLE lt     : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE rt     : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE res    : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE borrow : STD_LOGIC := '1';
        VARIABLE a,b,s1 : STD_LOGIC;
      BEGIN       
        lt := zxt( arg1, ml );
        rt := zxt( arg2, ml );
      
        FOR i IN res'reverse_range LOOP
          a := lt(i);
          b := NOT rt(i);
          s1 := a + b;
          res(i) := s1 + borrow;
          borrow := (a AND b) OR (s1 AND borrow);
        END LOOP;           
        RETURN res;                
      END "-";  

  FUNCTION "-" (arg1, arg2:UNSIGNED)         RETURN UNSIGNED IS
        CONSTANT ml     : INTEGER := maximum(arg1'length,arg2'length);
        VARIABLE lt     : UNSIGNED(1 TO ml);
        VARIABLE rt     : UNSIGNED(1 TO ml);
        VARIABLE res    : UNSIGNED(1 TO ml);
        VARIABLE borrow : STD_LOGIC := '1';
        VARIABLE a,b,s1 : STD_LOGIC;
      BEGIN       
        lt := zxt( arg1, ml );
        rt := zxt( arg2, ml );
      
        FOR i IN res'reverse_range LOOP
          a := lt(i);
          b := NOT rt(i);
          s1 := a + b;
          res(i) := s1 + borrow;
          borrow := (a AND b) OR (s1 AND borrow);
        END LOOP;           
        RETURN res;                
      END "-";  


  FUNCTION "-" (arg1, arg2:SIGNED)           RETURN SIGNED IS
    CONSTANT len : INTEGER := maximum(arg1'length,arg2'length) ;
    VARIABLE a,b : UNSIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
    VARIABLE answer : SIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
      BEGIN
         assert arg1'length > 1 AND arg2'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      a := (OTHERS => arg1(arg1'left)) ;
      a(arg1'length - 1 DOWNTO 0) := UNSIGNED(arg1);
      b := (OTHERS => arg2(arg2'left)) ;
      b(arg2'length - 1 DOWNTO 0) := UNSIGNED(arg2);
      answer := SIGNED( a - b );
      RETURN (answer);
  END ;

-----------------------------------------
-- Unary subtract and add Functions --
-----------------------------------------
  FUNCTION "+" (arg1:STD_ULOGIC_VECTOR) RETURN STD_ULOGIC_VECTOR IS
  BEGIN
    RETURN (arg1);
  END;
      
  FUNCTION "+" (arg1:STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
  BEGIN
    RETURN (arg1);
  END;
      
  FUNCTION "+" (arg1:UNSIGNED) RETURN UNSIGNED IS
  BEGIN
    RETURN (arg1);
  END;
      
  FUNCTION "+" (arg1:SIGNED) RETURN SIGNED IS
  BEGIN
    RETURN (arg1);
  END;
      
      FUNCTION hasx( v : SIGNED ) RETURN BOOLEAN IS
      BEGIN
         FOR i IN v'range LOOP
             IF v(i) = '0' OR v(i) = '1' OR v(i) = 'L' OR v(i) = 'H'THEN 
                 NULL;
             ELSE
                 RETURN TRUE;
             END IF;
         END LOOP;
         RETURN FALSE;
      END hasx;
      
  FUNCTION "-" (arg1:SIGNED)           RETURN SIGNED IS
    constant    len      : integer := arg1'length;
    VARIABLE    answer, tmp   : SIGNED( len-1 downto 0 ) := (others=>'0');
    VARIABLE    index    : integer := len;
  BEGIN
         assert arg1'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
    IF hasx(arg1) THEN
      answer := (OTHERS => 'X');
    ELSE
      tmp := arg1;
      lp1 : FOR i IN answer'REVERSE_RANGE LOOP
         IF (tmp(i) = '1' OR tmp(i) = 'H') THEN
           index := i+1;
           answer(i downto 0) := tmp(i downto 0);
           exit;
         END IF;
       END LOOP lp1;
       answer(len-1 downto index) := NOT tmp(len-1 downto index);
     end if;
   RETURN (answer);
  END ;

--------------------------------------------
-- Synthesizable multiplication Functions --
--------------------------------------------
      FUNCTION shift( v : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR IS
         VARIABLE v1 : STD_ULOGIC_VECTOR( v'range );
      BEGIN
         FOR i IN (v'left+1) TO v'right LOOP
             v1(i-1) := v(i);
         END LOOP;
         v1(v1'right) := '0';
         RETURN v1;
      END shift;
      
      PROCEDURE copy(a : IN STD_ULOGIC_VECTOR; b : OUT STD_ULOGIC_VECTOR) IS
         VARIABLE bi : INTEGER := b'right;
      BEGIN
         FOR i IN a'reverse_range LOOP
             b(bi) := a(i);
             bi := bi - 1;
         END LOOP;
      END copy;

      FUNCTION shift( v : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR IS
         VARIABLE v1 : STD_LOGIC_VECTOR( v'range );
      BEGIN
         FOR i IN (v'left+1) TO v'right LOOP
             v1(i-1) := v(i);
         END LOOP;
         v1(v1'right) := '0';
         RETURN v1;
      END shift;
      
      PROCEDURE copy(a : IN STD_LOGIC_VECTOR; b : OUT STD_LOGIC_VECTOR) IS
         VARIABLE bi : INTEGER := b'right;
      BEGIN
         FOR i IN a'reverse_range LOOP
             b(bi) := a(i);
             bi := bi - 1;
         END LOOP;
      END copy;

      FUNCTION shift( v : SIGNED ) RETURN SIGNED IS
         VARIABLE v1 : SIGNED( v'range );
      BEGIN
         FOR i IN (v'left+1) TO v'right LOOP
             v1(i-1) := v(i);
         END LOOP;
         v1(v1'right) := '0';
         RETURN v1;
      END shift;
      
      PROCEDURE copy(a : IN SIGNED; b : OUT SIGNED) IS
         VARIABLE bi : INTEGER := b'right;
      BEGIN
         FOR i IN a'reverse_range LOOP
             b(bi) := a(i);
             bi := bi - 1;
         END LOOP;
      END copy;

      FUNCTION shift( v : UNSIGNED ) RETURN UNSIGNED IS
         VARIABLE v1 : UNSIGNED( v'range );
      BEGIN
         FOR i IN (v'left+1) TO v'right LOOP
             v1(i-1) := v(i);
         END LOOP;
         v1(v1'right) := '0';
         RETURN v1;
      END shift;
      
      PROCEDURE copy(a : IN UNSIGNED; b : OUT UNSIGNED) IS
         VARIABLE bi : INTEGER := b'right;
      BEGIN
         FOR i IN a'reverse_range LOOP
             b(bi) := a(i);
             bi := bi - 1;
         END LOOP;
      END copy;
      
  FUNCTION "*" (arg1, arg2:STD_ULOGIC_VECTOR) RETURN STD_ULOGIC_VECTOR IS
        VARIABLE ml     : INTEGER := arg1'length + arg2'length;
        VARIABLE lt     : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE rt     : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE prod   : STD_ULOGIC_VECTOR(1 TO ml) := (OTHERS=>'0');
      BEGIN       
        lt := zxt( arg1, ml );
        rt := zxt( arg2, ml );
        FOR i IN rt'reverse_range LOOP
          IF rt(i) = '1' THEN 
            prod := prod + lt;
          END IF;
          lt := shift(lt);
        END LOOP;
        RETURN prod;
      END "*";  

  FUNCTION "*" (arg1, arg2:STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
        VARIABLE ml     : INTEGER := arg1'length + arg2'length;
        VARIABLE lt     : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE rt     : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE prod   : STD_LOGIC_VECTOR(1 TO ml) := (OTHERS=>'0');
      BEGIN       
        lt := zxt( arg1, ml );
        rt := zxt( arg2, ml );
        FOR i IN rt'reverse_range LOOP
          IF rt(i) = '1' THEN 
            prod := prod + lt;
          END IF;
          lt := shift(lt);
        END LOOP;
        RETURN prod;
      END "*";  

  FUNCTION "*" (arg1, arg2:UNSIGNED)         RETURN UNSIGNED IS
        VARIABLE ml     : INTEGER := arg1'length + arg2'length;
        VARIABLE lt     : UNSIGNED(1 TO ml);
        VARIABLE rt     : UNSIGNED(1 TO ml);
        VARIABLE prod   : UNSIGNED(1 TO ml) := (OTHERS=>'0');
      BEGIN       
        lt := zxt( arg1, ml );
        rt := zxt( arg2, ml );
        FOR i IN rt'reverse_range LOOP
          IF rt(i) = '1' THEN 
            prod := prod + lt;
          END IF;
          lt := shift(lt);
        END LOOP;
        RETURN prod;
      END "*";  
 
    --//// Sign Extend ////
    --
    -- Function sxt
    --
    FUNCTION sxt( q : SIGNED; i : INTEGER ) RETURN SIGNED IS
        VARIABLE qs : SIGNED (1 TO i);
        VARIABLE qt : SIGNED (1 TO q'length);
    BEGIN
        qt := q;
        IF i < q'length THEN
            qs := qt( (q'length-i+1) TO qt'right);
        ELSIF i > q'length THEN
            qs := (OTHERS=>q(q'left));
            qs := qs(1 TO (i-q'length)) & qt;
        ELSE
            qs := qt;
        END IF;
        RETURN qs;
    END;
    
    FUNCTION "*" (arg1, arg2:SIGNED)           RETURN SIGNED IS
        VARIABLE ml     : INTEGER := arg1'length + arg2'length;
        VARIABLE lt     : SIGNED(1 TO ml);
        VARIABLE rt     : SIGNED(1 TO ml);
        VARIABLE prod   : SIGNED(1 TO ml) := (OTHERS=>'0');
      BEGIN       
         assert arg1'length > 1 AND arg2'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
        lt := sxt( arg1, ml );
        rt := sxt( arg2, ml );
        FOR i IN rt'reverse_range LOOP
          IF rt(i) = '1' THEN 
            prod := prod + lt;
          END IF;
          lt := shift(lt);
        END LOOP;
        RETURN prod;
      END "*";  

      FUNCTION rshift( v : STD_ULOGIC_VECTOR ) RETURN STD_ULOGIC_VECTOR IS
         VARIABLE v1 : STD_ULOGIC_VECTOR( v'range );
      BEGIN
         FOR i IN v'left TO v'right-1 LOOP
             v1(i+1) := v(i);
         END LOOP;
         v1(v1'left) := '0';
         RETURN v1;
      END rshift;
      
      FUNCTION hasx( v : STD_ULOGIC_VECTOR ) RETURN BOOLEAN IS
      BEGIN
         FOR i IN v'range LOOP
             IF v(i) = '0' OR v(i) = '1' OR v(i) = 'L' OR v(i) = 'H'THEN 
                 NULL;
             ELSE
                 RETURN TRUE;
             END IF;
         END LOOP;
         RETURN FALSE;
      END hasx;
      
      FUNCTION rshift( v : STD_LOGIC_VECTOR ) RETURN STD_LOGIC_VECTOR IS
         VARIABLE v1 : STD_LOGIC_VECTOR( v'range );
      BEGIN
         FOR i IN v'left TO v'right-1 LOOP
             v1(i+1) := v(i);
         END LOOP;
         v1(v1'left) := '0';
         RETURN v1;
      END rshift;
      
      FUNCTION hasx( v : STD_LOGIC_VECTOR ) RETURN BOOLEAN IS
      BEGIN
         FOR i IN v'range LOOP
             IF v(i) = '0' OR v(i) = '1' OR v(i) = 'L' OR v(i) = 'H'THEN 
                 NULL;
             ELSE
                 RETURN TRUE;
             END IF;
         END LOOP;
         RETURN FALSE;
      END hasx;
      
      FUNCTION rshift( v : UNSIGNED ) RETURN UNSIGNED IS
         VARIABLE v1 : UNSIGNED( v'range );
      BEGIN
         FOR i IN v'left TO v'right-1 LOOP
             v1(i+1) := v(i);
         END LOOP;
         v1(v1'left) := '0';
         RETURN v1;
      END rshift;
      
      FUNCTION hasx( v : UNSIGNED ) RETURN BOOLEAN IS
      BEGIN
         FOR i IN v'range LOOP
             IF v(i) = '0' OR v(i) = '1' OR v(i) = 'L' OR v(i) = 'H'THEN 
                 NULL;
             ELSE
                 RETURN TRUE;
             END IF;
         END LOOP;
         RETURN FALSE;
      END hasx;
      
      FUNCTION rshift( v : SIGNED ) RETURN SIGNED IS
         VARIABLE v1 : SIGNED( v'range );
      BEGIN
         FOR i IN v'left TO v'right-1 LOOP
             v1(i+1) := v(i);
         END LOOP;
         v1(v1'left) := '0';
         RETURN v1;
      END rshift;
      
      FUNCTION "/" (l, r :STD_ULOGIC_VECTOR) RETURN STD_ULOGIC_VECTOR IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : STD_ULOGIC_VECTOR(0 TO ml+1);
        VARIABLE rt     : STD_ULOGIC_VECTOR(0 TO ml+1);
        VARIABLE quote  : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE tmp    : STD_ULOGIC_VECTOR(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : STD_ULOGIC_VECTOR(0 TO ml+1) := (OTHERS=>'0');
      
      BEGIN
         ASSERT NOT (r = "0")
             REPORT "Attempted divide by ZERO"
             SEVERITY ERROR;
         IF hasx(l) OR hasx(r) THEN
           FOR i IN quote'range LOOP
             quote(i) := 'X';
           END LOOP;
         ELSE
           lt := zxt( l, ml+2 );
           WHILE lt >= r LOOP
             rt := zxt( r, ml+2 );
             n := (OTHERS=>'0');
             n(n'right) := '1';
             WHILE rt <= lt LOOP
               rt := shift(rt);
               n  := shift(n);
             END LOOP;
             rt := rshift(rt);
             lt := lt - rt;
             n := rshift(n);
             tmp := tmp + n;
           END LOOP;
         END IF;
         quote := tmp(2 TO ml+1);
         RETURN quote;
      END "/";

      FUNCTION "/" (l, r :STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : STD_LOGIC_VECTOR(0 TO ml+1);
        VARIABLE rt     : STD_LOGIC_VECTOR(0 TO ml+1);
        VARIABLE quote  : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE tmp    : STD_LOGIC_VECTOR(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : STD_LOGIC_VECTOR(0 TO ml+1) := (OTHERS=>'0');
      
      BEGIN
         ASSERT NOT (r = "0")
             REPORT "Attempted divide by ZERO"
             SEVERITY ERROR;
         IF hasx(l) OR hasx(r) THEN
           FOR i IN quote'range LOOP
             quote(i) := 'X';
           END LOOP;
         ELSE
           lt := zxt( l, ml+2 );
           WHILE lt >= r LOOP
             rt := zxt( r, ml+2 );
             n := (OTHERS=>'0');
             n(n'right) := '1';
             WHILE rt <= lt LOOP
               rt := shift(rt);
               n  := shift(n);
             END LOOP;
             rt := rshift(rt);
             lt := lt - rt;
             n := rshift(n);
             tmp := tmp + n;
           END LOOP;
         END IF;
         quote := tmp(2 TO ml+1);
         RETURN quote;
      END "/";

      FUNCTION "/" (l, r :UNSIGNED) RETURN UNSIGNED IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : UNSIGNED(0 TO ml+1);
        VARIABLE rt     : UNSIGNED(0 TO ml+1);
        VARIABLE quote  : UNSIGNED(1 TO ml);
        VARIABLE tmp    : UNSIGNED(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : UNSIGNED(0 TO ml+1) := (OTHERS=>'0');
      
      BEGIN
         ASSERT NOT (r = "0")
             REPORT "Attempted divide by ZERO"
             SEVERITY ERROR;
         IF hasx(l) OR hasx(r) THEN
           FOR i IN quote'range LOOP
             quote(i) := 'X';
           END LOOP;
         ELSE
           lt := zxt( l, ml+2 );
           WHILE lt >= r LOOP
             rt := zxt( r, ml+2 );
             n := (OTHERS=>'0');
             n(n'right) := '1';
             WHILE rt <= lt LOOP
               rt := shift(rt);
               n  := shift(n);
             END LOOP;
             rt := rshift(rt);
             lt := lt - rt;
             n := rshift(n);
             tmp := tmp + n;
           END LOOP;
         END IF;
         quote := tmp(2 TO ml+1);
         RETURN quote;
      END "/";

      FUNCTION "/" (l, r :SIGNED) RETURN SIGNED IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : SIGNED(0 TO ml+1);
        VARIABLE rt     : SIGNED(0 TO ml+1);
        VARIABLE quote  : SIGNED(1 TO ml);
        VARIABLE tmp    : SIGNED(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : SIGNED(0 TO ml+1) := (OTHERS=>'0');
      
      BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
         ASSERT NOT (r = "0")
             REPORT "Attempted divide by ZERO"
             SEVERITY ERROR;
         IF hasx(l) OR hasx(r) THEN
           FOR i IN quote'range LOOP
             quote(i) := 'X';
           END LOOP;
         ELSE
           lt := sxt( l, ml+2 );
           WHILE lt >= r LOOP
             rt := sxt( r, ml+2 );
             n := (OTHERS=>'0');
             n(n'right) := '1';
             WHILE rt <= lt LOOP
               rt := shift(rt);
               n  := shift(n);
             END LOOP;
             rt := rshift(rt);
             lt := lt - rt;
             n := rshift(n);
             tmp := tmp + n;
           END LOOP;
         END IF;
         quote := tmp(2 TO ml+1);
         RETURN quote;
      END "/";

      FUNCTION "MOD" (l, r :STD_ULOGIC_VECTOR) RETURN STD_ULOGIC_VECTOR IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : STD_ULOGIC_VECTOR(0 TO ml+1);
        VARIABLE rt     : STD_ULOGIC_VECTOR(0 TO ml+1);
        VARIABLE quote  : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE tmp    : STD_ULOGIC_VECTOR(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : STD_ULOGIC_VECTOR(0 TO ml) := (OTHERS=>'0');
      
      BEGIN                              
        ASSERT NOT (r = "0")
          REPORT "Attempted divide by ZERO"
          SEVERITY ERROR;
        IF hasx(l) OR hasx(r) THEN
          FOR i IN lt'range LOOP
            lt(i) := 'X';
          END LOOP;
        ELSE
          lt := zxt( l, ml+2 );
          WHILE lt >= r LOOP
            rt := zxt( r, ml+2 );
            WHILE rt <= lt LOOP
              rt := shift(rt);
            END LOOP;
            rt := rshift(rt);
            lt := lt - rt;
          END LOOP;
        END IF;
        RETURN lt(2 TO ml+1);
      END "MOD";
      
      FUNCTION "MOD" (l, r :STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : STD_LOGIC_VECTOR(0 TO ml+1);
        VARIABLE rt     : STD_LOGIC_VECTOR(0 TO ml+1);
        VARIABLE quote  : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE tmp    : STD_LOGIC_VECTOR(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : STD_LOGIC_VECTOR(0 TO ml) := (OTHERS=>'0');
      
      BEGIN                              
        ASSERT NOT (r = "0")
          REPORT "Attempted divide by ZERO"
          SEVERITY ERROR;
        IF hasx(l) OR hasx(r) THEN
          FOR i IN lt'range LOOP
            lt(i) := 'X';
          END LOOP;
        ELSE
          lt := zxt( l, ml+2 );
          WHILE lt >= r LOOP
            rt := zxt( r, ml+2 );
            WHILE rt <= lt LOOP
              rt := shift(rt);
            END LOOP;
            rt := rshift(rt);
            lt := lt - rt;
          END LOOP;
        END IF;
        RETURN lt(2 TO ml+1);
      END "MOD";
      
      FUNCTION "MOD" (l, r :UNSIGNED) RETURN UNSIGNED IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : UNSIGNED(0 TO ml+1);
        VARIABLE rt     : UNSIGNED(0 TO ml+1);
        VARIABLE quote  : UNSIGNED(1 TO ml);
        VARIABLE tmp    : UNSIGNED(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : UNSIGNED(0 TO ml) := (OTHERS=>'0');
      
      BEGIN                              
        ASSERT NOT (r = "0")
          REPORT "Attempted divide by ZERO"
          SEVERITY ERROR;
        IF hasx(l) OR hasx(r) THEN
          FOR i IN lt'range LOOP
            lt(i) := 'X';
          END LOOP;
        ELSE
          lt := zxt( l, ml+2 );
          WHILE lt >= r LOOP
            rt := zxt( r, ml+2 );
            WHILE rt <= lt LOOP
              rt := shift(rt);
            END LOOP;
            rt := rshift(rt);
            lt := lt - rt;
          END LOOP;
        END IF;
        RETURN lt(2 TO ml+1);
      END "MOD";
      
      FUNCTION "REM" (l, r :STD_ULOGIC_VECTOR) RETURN STD_ULOGIC_VECTOR IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : STD_ULOGIC_VECTOR(0 TO ml+1);
        VARIABLE rt     : STD_ULOGIC_VECTOR(0 TO ml+1);
        VARIABLE quote  : STD_ULOGIC_VECTOR(1 TO ml);
        VARIABLE tmp    : STD_ULOGIC_VECTOR(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : STD_ULOGIC_VECTOR(0 TO ml) := (OTHERS=>'0');
      
      BEGIN                              
        ASSERT NOT (r = "0")
          REPORT "Attempted divide by ZERO"
          SEVERITY ERROR;
        IF hasx(l) OR hasx(r) THEN
          FOR i IN lt'range LOOP
            lt(i) := 'X';
          END LOOP;
        ELSE
          lt := zxt( l, ml+2 );
          WHILE lt >= r LOOP
            rt := zxt( r, ml+2 );
            WHILE rt <= lt LOOP
              rt := shift(rt);
            END LOOP;
            rt := rshift(rt);
            lt := lt - rt;
          END LOOP;
        END IF;
        RETURN lt(2 TO ml+1);
      END "REM";
      
      FUNCTION "REM" (l, r :STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : STD_LOGIC_VECTOR(0 TO ml+1);
        VARIABLE rt     : STD_LOGIC_VECTOR(0 TO ml+1);
        VARIABLE quote  : STD_LOGIC_VECTOR(1 TO ml);
        VARIABLE tmp    : STD_LOGIC_VECTOR(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : STD_LOGIC_VECTOR(0 TO ml) := (OTHERS=>'0');
      
      BEGIN                              
        ASSERT NOT (r = "0")
          REPORT "Attempted divide by ZERO"
          SEVERITY ERROR;
        IF hasx(l) OR hasx(r) THEN
          FOR i IN lt'range LOOP
            lt(i) := 'X';
          END LOOP;
        ELSE
          lt := zxt( l, ml+2 );
          WHILE lt >= r LOOP
            rt := zxt( r, ml+2 );
            WHILE rt <= lt LOOP
              rt := shift(rt);
            END LOOP;
            rt := rshift(rt);
            lt := lt - rt;
          END LOOP;
        END IF;
        RETURN lt(2 TO ml+1);
      END "REM";
      
      FUNCTION "REM" (l, r :UNSIGNED) RETURN UNSIGNED IS
      
        CONSTANT ml     : INTEGER := maximum(l'length,r'length);
        VARIABLE lt     : UNSIGNED(0 TO ml+1);
        VARIABLE rt     : UNSIGNED(0 TO ml+1);
        VARIABLE quote  : UNSIGNED(1 TO ml);
        VARIABLE tmp    : UNSIGNED(0 TO ml+1) := (OTHERS=>'0');
        VARIABLE n      : UNSIGNED(0 TO ml) := (OTHERS=>'0');
      
      BEGIN                              
        ASSERT NOT (r = "0")
          REPORT "Attempted divide by ZERO"
          SEVERITY ERROR;
        IF hasx(l) OR hasx(r) THEN
          FOR i IN lt'range LOOP
            lt(i) := 'X';
          END LOOP;
        ELSE
          lt := zxt( l, ml+2 );
          WHILE lt >= r LOOP
            rt := zxt( r, ml+2 );
            WHILE rt <= lt LOOP
              rt := shift(rt);
            END LOOP;
            rt := rshift(rt);
            lt := lt - rt;
          END LOOP;
        END IF;
        RETURN lt(2 TO ml+1);
      END "REM";
      
      FUNCTION "**" (l, r :STD_ULOGIC_VECTOR) RETURN STD_ULOGIC_VECTOR IS
      
        VARIABLE return_vector : STD_ULOGIC_VECTOR(l'range) := (OTHERS=>'0');
        VARIABLE tmp           : STD_ULOGIC_VECTOR(1 TO (2 * l'length)) := (OTHERS=>'0');
        CONSTANT lsh_l         : INTEGER := l'length+1;
        CONSTANT lsh_r         : INTEGER := 2 * l'length;
        VARIABLE pow           : INTEGER;
      
      BEGIN                              
         IF (hasx(l) OR hasx(r)) THEN
             FOR i IN return_vector'range LOOP
                 return_vector(i) := 'X';
             END LOOP;
         ELSE
             pow := to_integer( r, 0 );
             tmp( tmp'right ) := '1';
             FOR i IN 1 TO pow LOOP
                 tmp := tmp(lsh_l TO lsh_r) * l;
             END LOOP;
             return_vector := tmp(lsh_l TO lsh_r);
         END IF;
         RETURN return_vector;
      END "**";  

      FUNCTION "**" (l, r :STD_LOGIC_VECTOR) RETURN STD_LOGIC_VECTOR IS
      
        VARIABLE return_vector : STD_LOGIC_VECTOR(l'range) := (OTHERS=>'0');
        VARIABLE tmp           : STD_LOGIC_VECTOR(1 TO (2 * l'length)) := (OTHERS=>'0');
        CONSTANT lsh_l         : INTEGER := l'length+1;
        CONSTANT lsh_r         : INTEGER := 2 * l'length;
        VARIABLE pow           : INTEGER;
      
      BEGIN                              
         IF (hasx(l) OR hasx(r)) THEN
             FOR i IN return_vector'range LOOP
                 return_vector(i) := 'X';
             END LOOP;
         ELSE
             pow := to_integer( r, 0 );
             tmp( tmp'right ) := '1';
             FOR i IN 1 TO pow LOOP
                 tmp := tmp(lsh_l TO lsh_r) * l;
             END LOOP;
             return_vector := tmp(lsh_l TO lsh_r);
         END IF;
         RETURN return_vector;
      END "**";  

      FUNCTION "**" (l, r :UNSIGNED) RETURN UNSIGNED IS
      
        VARIABLE return_vector : UNSIGNED(l'range) := (OTHERS=>'0');
        VARIABLE tmp           : UNSIGNED(1 TO (2 * l'length)) := (OTHERS=>'0');
        CONSTANT lsh_l         : INTEGER := l'length+1;
        CONSTANT lsh_r         : INTEGER := 2 * l'length;
        VARIABLE pow           : INTEGER;
      
      BEGIN                              
         IF (hasx(l) OR hasx(r)) THEN
             FOR i IN return_vector'range LOOP
                 return_vector(i) := 'X';
             END LOOP;
         ELSE
             pow := to_integer( r, 0 );
             tmp( tmp'right ) := '1';
             FOR i IN 1 TO pow LOOP
                 tmp := tmp(lsh_l TO lsh_r) * l;
             END LOOP;
             return_vector := tmp(lsh_l TO lsh_r);
         END IF;
         RETURN return_vector;
      END "**";  

--
-- Absolute Value Functions
--
  FUNCTION "abs" (arg1:SIGNED)  RETURN SIGNED IS
    constant    len      : integer := arg1'length;
    VARIABLE    answer, tmp   : SIGNED( len-1 downto 0 ) := (others=>'0');
    VARIABLE    index    : integer := len;
  BEGIN
         assert arg1'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
    IF hasx(arg1) THEN
      answer := (OTHERS => 'X');
    ELSIF (arg1(arg1'left) = '0' OR arg1(arg1'left) = 'L') THEN
        answer := arg1;
    ELSE
      tmp := arg1;
      lp1 : FOR i IN answer'REVERSE_RANGE LOOP
         IF (tmp(i) = '1' OR tmp(i) = 'H') THEN
           index := i+1;
           answer(i downto 0) := tmp(i downto 0);
           exit;
         END IF;
       END LOOP lp1;
       answer(len-1 downto index) := NOT tmp(len-1 downto index);
     end if;
   RETURN (answer);
  END ;

--
-- Shift Left (arithmetic) Functions
--

  FUNCTION "sla" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : std_ulogic_vector(1 to len) := (others => arg1(arg1'right));
     VARIABLE ans : STD_ULOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(arg2+1 to len) & se(1 to arg2));
    END IF;
  END ;

  FUNCTION "sla" (arg1:STD_LOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : std_logic_vector(1 to len) := (others => arg1(arg1'right));
     VARIABLE ans : STD_LOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(arg2+1 to len) & se(1 to arg2));
    END IF;
  END ;

  FUNCTION "sla" (arg1:UNSIGNED ; arg2:NATURAL)  RETURN UNSIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : UNSIGNED(1 to len) := (others => arg1(arg1'right));
     VARIABLE ans : UNSIGNED(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(arg2+1 to len) & se(1 to arg2));
    END IF;
  END ;

  FUNCTION "sla" (arg1:SIGNED   ; arg2:NATURAL)  RETURN SIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : SIGNED(1 to len) := (others => arg1(arg1'right));
     VARIABLE ans : SIGNED(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(arg2+1 to len) & se(1 to arg2));
    END IF;
  END ;

--
-- Shift Right (arithmetics) Functions
--
  FUNCTION "sra" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : std_ulogic_vector(1 to len) := (others => arg1(arg1'left));
     VARIABLE ans : STD_ULOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (se(1 to arg2) & ans(1 to len-arg2));
    END IF;
  END ;

  FUNCTION "sra" (arg1:STD_LOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : std_logic_vector(1 to len) := (others => arg1(arg1'left));
     VARIABLE ans : STD_LOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (se(1 to arg2) & ans(1 to len-arg2));
    END IF;
  END ;

  FUNCTION "sra" (arg1:UNSIGNED ; arg2:NATURAL)  RETURN UNSIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : UNSIGNED(1 to len) := (others => arg1(arg1'left));
     VARIABLE ans : UNSIGNED(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (se(1 to arg2) & ans(1 to len-arg2));
    END IF;
  END ;

  FUNCTION "sra" (arg1:SIGNED   ; arg2:NATURAL)  RETURN SIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : SIGNED(1 to len) := (others => arg1(arg1'left));
     VARIABLE ans : SIGNED(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (se(1 to arg2) & ans(1 to len-arg2));
    END IF;
  END ;

--
-- Shift Left (logical) Functions
--

  FUNCTION "sll" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : std_ulogic_vector(1 to len) := (others =>'0');
     VARIABLE ans : STD_ULOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(arg2+1 to len) & se(1 to arg2));
    END IF;
  END ;

  FUNCTION "sll" (arg1:STD_LOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : std_logic_vector(1 to len) := (others =>'0');
     VARIABLE ans : STD_LOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(arg2+1 to len) & se(1 to arg2));
    END IF;
  END ;

  FUNCTION "sll" (arg1:UNSIGNED ; arg2:NATURAL)  RETURN UNSIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : UNSIGNED(1 to len) := (others =>'0');
     VARIABLE ans : UNSIGNED(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(arg2+1 to len) & se(1 to arg2));
    END IF;
  END ;

  FUNCTION "sll" (arg1:SIGNED   ; arg2:NATURAL)  RETURN SIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : SIGNED(1 to len) := (others =>'0');
     VARIABLE ans : SIGNED(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(arg2+1 to len) & se(1 to arg2));
    END IF;
  END ;

--
-- Shift Right (logical) Functions
--
  FUNCTION "srl" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : std_ulogic_vector(1 to len) := (others => '0');
     VARIABLE ans : STD_ULOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (se(1 to arg2) & ans(1 to len-arg2));
    END IF;
  END ;

  FUNCTION "srl" (arg1:STD_LOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : std_logic_vector(1 to len) := (others => '0');
     VARIABLE ans : STD_LOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (se(1 to arg2) & ans(1 to len-arg2));
    END IF;
  END ;

  FUNCTION "srl" (arg1:UNSIGNED ; arg2:NATURAL)  RETURN UNSIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : UNSIGNED(1 to len) := (others => '0');
     VARIABLE ans : UNSIGNED(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (se(1 to arg2) & ans(1 to len-arg2));
    END IF;
  END ;

  FUNCTION "srl" (arg1:SIGNED   ; arg2:NATURAL)  RETURN SIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT se : SIGNED(1 to len) := (others => '0');
     VARIABLE ans : SIGNED(1 to len) := arg1;
  BEGIN
    IF (arg2 >= len) THEN
      RETURN (se);
    ELSIF (arg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (se(1 to arg2) & ans(1 to len-arg2));
    END IF;
  END ;

--
-- Rotate Left (Logical) Functions
--
  FUNCTION "rol" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT marg2 : integer := arg2 mod len;
     VARIABLE ans : STD_ULOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (marg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(marg2+1 to len) & ans(1 to marg2));
    END IF;
  END ;

  FUNCTION "rol" (arg1:STD_LOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT marg2 : integer := arg2 mod len;
     VARIABLE ans : STD_LOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (marg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(marg2+1 to len) & ans(1 to marg2));
    END IF;
  END ;

  FUNCTION "rol" (arg1:UNSIGNED ; arg2:NATURAL)  RETURN UNSIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT marg2 : integer := arg2 mod len;
     VARIABLE ans : UNSIGNED(1 to len) := arg1;
  BEGIN
    IF (marg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(marg2+1 to len) & ans(1 to marg2));
    END IF;
  END ;

  FUNCTION "rol" (arg1:SIGNED   ; arg2:NATURAL)  RETURN SIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT marg2 : integer := arg2 mod len;
     VARIABLE ans : SIGNED(1 to len) := arg1;
  BEGIN
    IF (marg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(marg2+1 to len) & ans(1 to marg2));
    END IF;
  END ;

--
-- Rotate Right (Logical) Functions
--
  FUNCTION "ror" (arg1:STD_ULOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_ULOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT marg2 : integer := arg2 mod len;
     VARIABLE ans : STD_ULOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (marg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(len-marg2+1 to len) & ans(1 to len-marg2));
    END IF;
  END ;

  FUNCTION "ror" (arg1:STD_LOGIC_VECTOR ; arg2:NATURAL)  RETURN STD_LOGIC_VECTOR IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT marg2 : integer := arg2 mod len;
     VARIABLE ans : STD_LOGIC_VECTOR(1 to len) := arg1;
  BEGIN
    IF (marg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(len-marg2+1 to len) & ans(1 to len-marg2));
    END IF;
  END ;

  FUNCTION "ror" (arg1:UNSIGNED ; arg2:NATURAL)  RETURN UNSIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT marg2 : integer := arg2 mod len;
     VARIABLE ans : UNSIGNED(1 to len) := arg1;
  BEGIN
    IF (marg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(len-marg2+1 to len) & ans(1 to len-marg2));
    END IF;
  END ;

  FUNCTION "ror" (arg1:SIGNED   ; arg2:NATURAL)  RETURN SIGNED IS
     CONSTANT len : INTEGER := arg1'length ;
     CONSTANT marg2 : integer := arg2 mod len;
     VARIABLE ans : SIGNED(1 to len) := arg1;
  BEGIN
    IF (marg2 = 0) THEN
      RETURN (arg1);
    ELSE
      RETURN (ans(len-marg2+1 to len) & ans(1 to len-marg2));
    END IF;
  END ;

--
-- Equal functions.
--
    CONSTANT eq_table : stdlogic_boolean_table := (
    --      ----------------------------------------------------------------------------
    --      |  U       X      0     1      Z      W      L      H      D         |   |  
    --      ----------------------------------------------------------------------------
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | U |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | X |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | 0 |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | 1 |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | Z |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | W |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | L |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | H |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE )   -- | D |
    );

    FUNCTION eq  ( l, r : STD_LOGIC ) RETURN BOOLEAN IS
    BEGIN
        RETURN eq_table( l, r );
    END;

    FUNCTION eq  ( l,r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        VARIABLE rt  : STD_ULOGIC_VECTOR ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               RETURN FALSE;
            END IF;
        END LOOP;
        RETURN TRUE;
    END;

    FUNCTION eq  ( l,r : STD_LOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : STD_LOGIC_VECTOR ( 1 TO ml );
        VARIABLE rt  : STD_LOGIC_VECTOR ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               RETURN FALSE;
            END IF;
        END LOOP;
        RETURN TRUE;
    END;

    FUNCTION eq  ( l,r : UNSIGNED ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : UNSIGNED ( 1 TO ml );
        VARIABLE rt  : UNSIGNED ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               RETURN FALSE;
            END IF;
        END LOOP;
        RETURN TRUE;
    END;

    FUNCTION eq  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      RETURN (eq( lt, rt ));
    END;

    FUNCTION "="  ( l,r : UNSIGNED ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : UNSIGNED ( 1 TO ml );
        VARIABLE rt  : UNSIGNED ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               RETURN FALSE;
            END IF;
        END LOOP;
        RETURN TRUE;
    END;

    FUNCTION "="  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      RETURN (eq( lt, rt ));
    END;

--
-- Not Equal function.
--
  CONSTANT neq_table : stdlogic_boolean_table := (
  --      ----------------------------------------------------------------------------
  --      |  U       X      0     1      Z      W      L      H      D         |   |  
  --      ----------------------------------------------------------------------------
  ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | U |
  ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | X |
  ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | 0 |
  ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | 1 |
  ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | Z |
  ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | W |
  ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | L |
  ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | H |
  ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE )   -- | D |
  );


    FUNCTION ne  ( l, r : STD_LOGIC ) RETURN BOOLEAN IS
    BEGIN
        RETURN neq_table( l, r );
    END;

    FUNCTION ne  ( l,r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        VARIABLE rt  : STD_ULOGIC_VECTOR ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF ne( lt(i), rt(i) ) THEN
               RETURN TRUE;
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION ne  ( l,r : STD_LOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : STD_LOGIC_VECTOR ( 1 TO ml );
        VARIABLE rt  : STD_LOGIC_VECTOR ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF ne( lt(i), rt(i) ) THEN
               RETURN TRUE;
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION ne  ( l,r : UNSIGNED ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : UNSIGNED ( 1 TO ml );
        VARIABLE rt  : UNSIGNED ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF ne( lt(i), rt(i) ) THEN
               RETURN TRUE;
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION ne  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      RETURN (ne( lt, rt ));
    END;

    FUNCTION "/=" ( l,r : UNSIGNED ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : UNSIGNED ( 1 TO ml );
        VARIABLE rt  : UNSIGNED ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF ne( lt(i), rt(i) ) THEN
               RETURN TRUE;
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION "/="  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      RETURN (ne( lt, rt ));
    END;

--
-- Less Than functions.
--
    CONSTANT ltb_table : stdlogic_boolean_table := (
    --      ----------------------------------------------------------------------------
    --      |  U       X      0     1      Z      W      L      H      D         |   |  
    --      ----------------------------------------------------------------------------
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | U |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | X |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | 0 |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | 1 |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | Z |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | W |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | L |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | H |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE )   -- | D |
    );

    FUNCTION lt  ( l, r : STD_LOGIC ) RETURN BOOLEAN IS
    BEGIN
        RETURN ltb_table( l, r );
    END;

    FUNCTION lt  ( l,r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE ltt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        VARIABLE rtt  : STD_ULOGIC_VECTOR ( 1 TO ml );
    BEGIN
        ltt := zxt( l, ml );
        rtt := zxt( r, ml );
        FOR i IN ltt'range LOOP
            IF NOT eq( ltt(i), rtt(i) ) THEN
               RETURN lt( ltt(i), rtt(i) );
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION lt  ( l,r : STD_LOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE ltt  : STD_LOGIC_VECTOR ( 1 TO ml );
        VARIABLE rtt  : STD_LOGIC_VECTOR ( 1 TO ml );
    BEGIN
        ltt := zxt( l, ml );
        rtt := zxt( r, ml );
        FOR i IN ltt'range LOOP
            IF NOT eq( ltt(i), rtt(i) ) THEN
               RETURN lt( ltt(i), rtt(i) );
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION lt  ( l,r : UNSIGNED ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE ltt  : UNSIGNED ( 1 TO ml );
        VARIABLE rtt  : UNSIGNED ( 1 TO ml );
    BEGIN
        ltt := zxt( l, ml );
        rtt := zxt( r, ml );
        FOR i IN ltt'range LOOP
            IF NOT eq( ltt(i), rtt(i) ) THEN
               RETURN lt( ltt(i), rtt(i) );
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION lt  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE ltt, rtt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      ltt := (OTHERS => l(l'left)) ;
      ltt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rtt := (OTHERS => r(r'left)) ;
      rtt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      IF(ltt(ltt'left) = '1' AND rtt(rtt'left) = '0') THEN
        RETURN(TRUE) ;
      ELSIF(ltt(ltt'left) = '0' AND rtt(rtt'left) = '1') THEN
        RETURN(FALSE) ;
      ELSE          
        RETURN (lt( ltt, rtt ));
      END IF ;
    END;

    FUNCTION "<"  ( l,r : UNSIGNED ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE ltt  : UNSIGNED ( 1 TO ml );
        VARIABLE rtt  : UNSIGNED ( 1 TO ml );
    BEGIN
        ltt := zxt( l, ml );
        rtt := zxt( r, ml );
        FOR i IN ltt'range LOOP
            IF NOT eq( ltt(i), rtt(i) ) THEN
               RETURN lt( ltt(i), rtt(i) );
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION "<"  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE ltt, rtt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      ltt := (OTHERS => l(l'left)) ;
      ltt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rtt := (OTHERS => r(r'left)) ;
      rtt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      IF(ltt(ltt'left) = '1' AND rtt(rtt'left) = '0') THEN
        RETURN(TRUE) ;
      ELSIF(ltt(ltt'left) = '0' AND rtt(rtt'left) = '1') THEN
        RETURN(FALSE) ;
      ELSE          
        RETURN (lt( ltt, rtt ));
      END IF ;
    END;

--
-- Greater Than functions.
--
    CONSTANT gtb_table : stdlogic_boolean_table := (
    --      ----------------------------------------------------------------------------
    --      |  U       X      0     1      Z      W      L      H      D         |   |  
    --      ----------------------------------------------------------------------------
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | U |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | X |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | 0 |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | 1 |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | Z |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | W |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE ),  -- | L |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | H |
    ( FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE )   -- | D |
    );

    FUNCTION gt  ( l, r : std_logic ) RETURN BOOLEAN IS
    BEGIN
        RETURN gtb_table( l, r );
    END ;

    FUNCTION gt  ( l,r : STD_ULOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : STD_ULOGIC_VECTOR ( 1 TO ml );
        VARIABLE rt  : STD_ULOGIC_VECTOR ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               RETURN gt( lt(i), rt(i) );
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION gt  ( l,r : STD_LOGIC_VECTOR ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : STD_LOGIC_VECTOR ( 1 TO ml );
        VARIABLE rt  : STD_LOGIC_VECTOR ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               RETURN gt( lt(i), rt(i) );
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION gt  ( l,r : UNSIGNED ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : UNSIGNED ( 1 TO ml );
        VARIABLE rt  : UNSIGNED ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               RETURN gt( lt(i), rt(i) );
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION gt  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      IF(lt(lt'left) = '1' AND rt(rt'left) = '0') THEN
        RETURN(FALSE) ;
      ELSIF(lt(lt'left) = '0' AND rt(rt'left) = '1') THEN
        RETURN(TRUE) ;
      ELSE          
        RETURN (gt( lt, rt ));
      END IF ;
    END;

    FUNCTION ">"  ( l,r : UNSIGNED ) RETURN BOOLEAN IS
        CONSTANT ml  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt  : UNSIGNED ( 1 TO ml );
        VARIABLE rt  : UNSIGNED ( 1 TO ml );
    BEGIN
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'range LOOP
            IF NOT eq( lt(i), rt(i) ) THEN
               RETURN gt( lt(i), rt(i) );
            END IF;
        END LOOP;
        RETURN FALSE;
    END;

    FUNCTION ">"  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      IF(lt(lt'left) = '1' AND rt(rt'left) = '0') THEN
        RETURN(FALSE) ;
      ELSIF(lt(lt'left) = '0' AND rt(rt'left) = '1') THEN
        RETURN(TRUE) ;
      ELSE          
        RETURN (gt( lt, rt ));
      END IF ;
    END;

--
-- Less Than or Equal to  functions.
--
    CONSTANT leb_table : stdlogic_boolean_table := (
    --      ----------------------------------------------------------------------------
    --      |  U       X      0     1      Z      W      L      H      D         |   |  
    --      ----------------------------------------------------------------------------
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | U |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | X |
    ( TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE ),  -- | 0 |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | 1 |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | Z |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | W |
    ( TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE ),  -- | L |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE ),  -- | H |
    ( FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE )   -- | D |
    );

    FUNCTION le  ( l, r : std_logic ) RETURN BOOLEAN IS
    BEGIN
        RETURN leb_table( l, r );
    END ;

    TYPE std_ulogic_fuzzy_state IS ('U', 'X', 'T', 'F', 'N');
    TYPE std_ulogic_fuzzy_state_table IS ARRAY ( std_ulogic, std_ulogic ) OF std_ulogic_fuzzy_state;

    CONSTANT le_fuzzy_table : std_ulogic_fuzzy_state_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |  
	--      ----------------------------------------------------
                ( 'U', 'U', 'U', 'N', 'U', 'U', 'U', 'N', 'U' ),  -- | U |
	        ( 'U', 'X', 'X', 'N', 'X', 'X', 'X', 'N', 'X' ),  -- | X |
	        ( 'N', 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N' ),  -- | 0 |
	        ( 'U', 'X', 'F', 'N', 'X', 'X', 'F', 'N', 'X' ),  -- | 1 |
	        ( 'U', 'X', 'X', 'N', 'X', 'X', 'X', 'N', 'X' ),  -- | Z |
	        ( 'U', 'X', 'X', 'N', 'X', 'X', 'X', 'N', 'X' ),  -- | W |
	        ( 'N', 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N' ),  -- | L |
	        ( 'U', 'X', 'F', 'N', 'X', 'X', 'F', 'N', 'X' ),  -- | H |
	        ( 'U', 'X', 'X', 'N', 'X', 'X', 'X', 'N', 'X' )   -- | D |
    );

    FUNCTION le  ( L,R : std_ulogic_vector ) RETURN boolean IS
        CONSTANT ml  : integer := maximum( L'LENGTH, R'LENGTH );
        VARIABLE lt  : std_ulogic_vector ( 1 to ml );
        VARIABLE rt  : std_ulogic_vector ( 1 to ml );
        VARIABLE res : std_ulogic_fuzzy_state;
    begin
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'RANGE LOOP
            res := le_fuzzy_table( lt(i), rt(i) );
            CASE res IS
              WHEN 'U'          => RETURN FALSE;
              WHEN 'X'          => RETURN FALSE;
              WHEN 'T'          => RETURN TRUE;
              WHEN 'F'          => RETURN FALSE;
              WHEN OTHERS       => null;
            END CASE;
        END LOOP;
        RETURN TRUE;
    end ;

    TYPE std_logic_fuzzy_state IS ('U', 'X', 'T', 'F', 'N');
    TYPE std_logic_fuzzy_state_table IS ARRAY ( std_logic, std_logic ) OF std_logic_fuzzy_state;

    CONSTANT le_lfuzzy_table : std_logic_fuzzy_state_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |  
	--      ----------------------------------------------------
                ( 'U', 'U', 'U', 'N', 'U', 'U', 'U', 'N', 'U' ),  -- | U |
	        ( 'U', 'X', 'X', 'N', 'X', 'X', 'X', 'N', 'X' ),  -- | X |
	        ( 'N', 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N' ),  -- | 0 |
	        ( 'U', 'X', 'F', 'N', 'X', 'X', 'F', 'N', 'X' ),  -- | 1 |
	        ( 'U', 'X', 'X', 'N', 'X', 'X', 'X', 'N', 'X' ),  -- | Z |
	        ( 'U', 'X', 'X', 'N', 'X', 'X', 'X', 'N', 'X' ),  -- | W |
	        ( 'N', 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N' ),  -- | L |
	        ( 'U', 'X', 'F', 'N', 'X', 'X', 'F', 'N', 'X' ),  -- | H |
	        ( 'U', 'X', 'X', 'N', 'X', 'X', 'X', 'N', 'X' )   -- | D |
    );

    FUNCTION le  ( L,R : std_logic_vector ) RETURN boolean IS
        CONSTANT ml  : integer := maximum( L'LENGTH, R'LENGTH );
        VARIABLE lt  : std_logic_vector ( 1 to ml );
        VARIABLE rt  : std_logic_vector ( 1 to ml );
        VARIABLE res : std_logic_fuzzy_state;
    begin
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'RANGE LOOP
            res := le_lfuzzy_table( lt(i), rt(i) );
            CASE res IS
              WHEN 'U'          => RETURN FALSE;
              WHEN 'X'          => RETURN FALSE;
              WHEN 'T'          => RETURN TRUE;
              WHEN 'F'          => RETURN FALSE;
              WHEN OTHERS       => null;
            END CASE;
        END LOOP;
        RETURN TRUE;
    end ;

    FUNCTION le  ( L,R : UNSIGNED ) RETURN boolean IS
        CONSTANT ml  : integer := maximum( L'LENGTH, R'LENGTH );
        VARIABLE lt  : UNSIGNED ( 1 to ml );
        VARIABLE rt  : UNSIGNED ( 1 to ml );
        VARIABLE res : std_logic_fuzzy_state;
    begin
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'RANGE LOOP
            res := le_lfuzzy_table( lt(i), rt(i) );
            CASE res IS
              WHEN 'U'          => RETURN FALSE;
              WHEN 'X'          => RETURN FALSE;
              WHEN 'T'          => RETURN TRUE;
              WHEN 'F'          => RETURN FALSE;
              WHEN OTHERS       => null;
            END CASE;
        END LOOP;
        RETURN TRUE;
    end ;

  FUNCTION le (l, r:SIGNED)           RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      IF(lt(lt'left) = '1' AND rt(rt'left) = '0') THEN
        RETURN(TRUE) ;
      ELSIF(lt(lt'left) = '0' AND rt(rt'left) = '1') THEN
        RETURN(FALSE) ;
      ELSE          
        RETURN (le( lt, rt ));
      END IF ;
    END;

    FUNCTION "<="  ( L,R : UNSIGNED ) RETURN boolean IS
        CONSTANT ml  : integer := maximum( L'LENGTH, R'LENGTH );
        VARIABLE lt  : UNSIGNED ( 1 to ml );
        VARIABLE rt  : UNSIGNED ( 1 to ml );
        VARIABLE res : std_logic_fuzzy_state;
    begin
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'RANGE LOOP
            res := le_lfuzzy_table( lt(i), rt(i) );
            CASE res IS
              WHEN 'U'          => RETURN FALSE;
              WHEN 'X'          => RETURN FALSE;
              WHEN 'T'          => RETURN TRUE;
              WHEN 'F'          => RETURN FALSE;
              WHEN OTHERS       => null;
            END CASE;
        END LOOP;
        RETURN TRUE;
    end ;

  FUNCTION "<=" (l, r:SIGNED)           RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      IF(lt(lt'left) = '1' AND rt(rt'left) = '0') THEN
        RETURN(TRUE) ;
      ELSIF(lt(lt'left) = '0' AND rt(rt'left) = '1') THEN
        RETURN(FALSE) ;
      ELSE          
        RETURN (le( lt, rt ));
      END IF ;
    END;

--
-- Greater Than or Equal to  functions.
--
    CONSTANT geb_table : stdlogic_boolean_table := (
    --      ----------------------------------------------------------------------------
    --      |  U       X      0     1      Z      W      L      H      D         |   |  
    --      ----------------------------------------------------------------------------
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | U |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | X |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | 0 |
    ( TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE ),  -- | 1 |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | Z |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | W |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE ),  -- | L |
    ( TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE ),  -- | H |
    ( FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE )   -- | D |
    );

    FUNCTION ge  ( l, r : std_logic ) RETURN BOOLEAN IS
    BEGIN
        RETURN geb_table( l, r );
    END ;

    CONSTANT ge_fuzzy_table : std_ulogic_fuzzy_state_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |  
	--      ----------------------------------------------------
                ( 'U', 'U', 'N', 'U', 'U', 'U', 'N', 'U', 'U' ),  -- | U |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' ),  -- | X |
	        ( 'U', 'X', 'N', 'F', 'X', 'X', 'N', 'F', 'X' ),  -- | 0 |
	        ( 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N', 'N' ),  -- | 1 |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' ),  -- | Z |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' ),  -- | W |
	        ( 'U', 'X', 'N', 'F', 'X', 'X', 'N', 'F', 'X' ),  -- | L |
	        ( 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N', 'N' ),  -- | H |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' )   -- | D |
    );

    FUNCTION ge  ( L,R : std_ulogic_vector ) RETURN boolean IS
        CONSTANT ml  : integer := maximum( L'LENGTH, R'LENGTH );
        VARIABLE lt  : std_ulogic_vector ( 1 to ml );
        VARIABLE rt  : std_ulogic_vector ( 1 to ml );
        VARIABLE res : std_ulogic_fuzzy_state;
    begin
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'RANGE LOOP
            res := ge_fuzzy_table( lt(i), rt(i) );
            CASE res IS
              WHEN 'U'          => RETURN FALSE;
              WHEN 'X'          => RETURN FALSE;
              WHEN 'T'          => RETURN TRUE;
              WHEN 'F'          => RETURN FALSE;
              WHEN OTHERS       => null;
            END CASE;
        END LOOP;
        RETURN TRUE;
    end ;

    CONSTANT ge_lfuzzy_table : std_logic_fuzzy_state_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |  
	--      ----------------------------------------------------
                ( 'U', 'U', 'N', 'U', 'U', 'U', 'N', 'U', 'U' ),  -- | U |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' ),  -- | X |
	        ( 'U', 'X', 'N', 'F', 'X', 'X', 'N', 'F', 'X' ),  -- | 0 |
	        ( 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N', 'N' ),  -- | 1 |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' ),  -- | Z |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' ),  -- | W |
	        ( 'U', 'X', 'N', 'F', 'X', 'X', 'N', 'F', 'X' ),  -- | L |
	        ( 'N', 'N', 'T', 'N', 'N', 'N', 'T', 'N', 'N' ),  -- | H |
	        ( 'U', 'X', 'N', 'X', 'X', 'X', 'N', 'X', 'X' )   -- | D |
    );

    FUNCTION ge  ( L,R : std_logic_vector ) RETURN boolean IS
        CONSTANT ml  : integer := maximum( L'LENGTH, R'LENGTH );
        VARIABLE lt  : std_logic_vector ( 1 to ml );
        VARIABLE rt  : std_logic_vector ( 1 to ml );
        VARIABLE res : std_logic_fuzzy_state;
    begin
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'RANGE LOOP
            res := ge_lfuzzy_table( lt(i), rt(i) );
            CASE res IS
              WHEN 'U'          => RETURN FALSE;
              WHEN 'X'          => RETURN FALSE;
              WHEN 'T'          => RETURN TRUE;
              WHEN 'F'          => RETURN FALSE;
              WHEN OTHERS       => null;
            END CASE;
        END LOOP;
        RETURN TRUE;
    end ;

    FUNCTION ge  ( L,R : UNSIGNED ) RETURN boolean IS
        CONSTANT ml  : integer := maximum( L'LENGTH, R'LENGTH );
        VARIABLE lt  : UNSIGNED ( 1 to ml );
        VARIABLE rt  : UNSIGNED ( 1 to ml );
        VARIABLE res : std_logic_fuzzy_state;
    begin
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'RANGE LOOP
            res := ge_lfuzzy_table( lt(i), rt(i) );
            CASE res IS
              WHEN 'U'          => RETURN FALSE;
              WHEN 'X'          => RETURN FALSE;
              WHEN 'T'          => RETURN TRUE;
              WHEN 'F'          => RETURN FALSE;
              WHEN OTHERS       => null;
            END CASE;
        END LOOP;
        RETURN TRUE;
    end ;

    FUNCTION ge  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      IF(lt(lt'left) = '1' AND rt(rt'left) = '0') THEN
        RETURN(FALSE) ;
      ELSIF(lt(lt'left) = '0' AND rt(rt'left) = '1') THEN
        RETURN(TRUE) ;
      ELSE          
        RETURN (ge( lt, rt ));
      END IF ;
    END;

    FUNCTION ">="  ( L,R : UNSIGNED ) RETURN boolean IS
        CONSTANT ml  : integer := maximum( L'LENGTH, R'LENGTH );
        VARIABLE lt  : UNSIGNED ( 1 to ml );
        VARIABLE rt  : UNSIGNED ( 1 to ml );
        VARIABLE res : std_logic_fuzzy_state;
    begin
        lt := zxt( l, ml );
        rt := zxt( r, ml );
        FOR i IN lt'RANGE LOOP
            res := ge_lfuzzy_table( lt(i), rt(i) );
            CASE res IS
              WHEN 'U'          => RETURN FALSE;
              WHEN 'X'          => RETURN FALSE;
              WHEN 'T'          => RETURN TRUE;
              WHEN 'F'          => RETURN FALSE;
              WHEN OTHERS       => null;
            END CASE;
        END LOOP;
        RETURN TRUE;
    end ;

    FUNCTION ">="  ( l,r : SIGNED ) RETURN BOOLEAN IS
        CONSTANT len  : INTEGER := maximum( l'length, r'length );
        VARIABLE lt, rt  : UNSIGNED ( len-1 downto 0 ) := (OTHERS => '0');
    BEGIN
         assert l'length > 1 AND r'length > 1
           report "SIGNED vector must be atleast 2 bits wide"
           severity ERROR;
      lt := (OTHERS => l(l'left)) ;
      lt(l'length - 1 DOWNTO 0) := UNSIGNED(l);
      rt := (OTHERS => r(r'left)) ;
      rt(r'length - 1 DOWNTO 0) := UNSIGNED(r);
      IF(lt(lt'left) = '1' AND rt(rt'left) = '0') THEN
        RETURN(FALSE) ;
      ELSIF(lt(lt'left) = '0' AND rt(rt'left) = '1') THEN
        RETURN(TRUE) ;
      ELSE          
        RETURN (ge( lt, rt ));
      END IF ;
    END;

   -------------------------------------------------------------------------------
   --  Logical Operations
   -------------------------------------------------------------------------------

	-- truth table for "and" function
    CONSTANT and_table : stdlogic_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |  
	--      ----------------------------------------------------
            ( 'U', 'U', '0', 'U', 'U', 'U', '0', 'U', 'U' ),  -- | U |
            ( 'U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' ),  -- | X |
            ( '0', '0', '0', '0', '0', '0', '0', '0', '0' ),  -- | 0 |
            ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | 1 |
   	        ( 'U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' ),  -- | Z |
   	        ( 'U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' ),  -- | W |
   	        ( '0', '0', '0', '0', '0', '0', '0', '0', '0' ),  -- | L |
   	        ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | H |
   	        ( 'U', 'X', '0', 'X', 'X', 'X', '0', 'X', 'X' )   -- | D |
    );

	-- truth table for "or" function
	CONSTANT or_table : stdlogic_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |  
	--      ----------------------------------------------------
                ( 'U', 'U', 'U', '1', 'U', 'U', 'U', '1', 'U' ),  -- | U |
	        ( 'U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' ),  -- | X |
	        ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | 0 |
	        ( '1', '1', '1', '1', '1', '1', '1', '1', '1' ),  -- | 1 |
	        ( 'U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' ),  -- | Z |
	        ( 'U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' ),  -- | W |
	        ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | L |
	        ( '1', '1', '1', '1', '1', '1', '1', '1', '1' ),  -- | H |
	        ( 'U', 'X', 'X', '1', 'X', 'X', 'X', '1', 'X' )   -- | D |
    );


	-- truth table for "xor" function
	CONSTANT xor_table : stdlogic_table := (
	--      ----------------------------------------------------
	--      |  U    X    0    1    Z    W    L    H    D         |   |  
	--      ----------------------------------------------------
                ( 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U', 'U' ),  -- | U |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | X |
	        ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | 0 |
	        ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', 'X' ),  -- | 1 |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | Z |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' ),  -- | W |
	        ( 'U', 'X', '0', '1', 'X', 'X', '0', '1', 'X' ),  -- | L |
	        ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', 'X' ),  -- | H |
	        ( 'U', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X' )   -- | D |
    );

	-- truth table for "not" function
	CONSTANT not_table: stdlogic_1D := 
	--  -------------------------------------------------
	--  |   U    X    0    1    Z    W    L    H    D   |
	--  -------------------------------------------------
	     ( 'U', 'X', '1', '0', 'X', 'X', '1', '0', 'X' ); 

     FUNCTION "and"  ( arg1,arg2 : UNSIGNED ) RETURN UNSIGNED IS
         CONSTANT ml  : integer := maximum( arg1'LENGTH, arg2'LENGTH );
         VARIABLE lt  : UNSIGNED ( 1 to ml );
         VARIABLE rt  : UNSIGNED ( 1 to ml );
         VARIABLE res : UNSIGNED ( 1 to ml );
     begin
         lt := zxt( arg1, ml );
         rt := zxt( arg2, ml );
         FOR i IN res'RANGE LOOP
             res(i) := and_table( lt(i), rt(i) );
         END LOOP;
         RETURN res; 
     end "and";

     FUNCTION "nand"  ( arg1,arg2 : UNSIGNED ) RETURN UNSIGNED IS
         CONSTANT ml  : integer := maximum( arg1'LENGTH, arg2'LENGTH );
         VARIABLE lt  : UNSIGNED ( 1 to ml );
         VARIABLE rt  : UNSIGNED ( 1 to ml );
         VARIABLE res : UNSIGNED ( 1 to ml );
     begin
         lt := zxt( arg1, ml );
         rt := zxt( arg2, ml );
         FOR i IN res'RANGE LOOP
             res(i) := not_table( and_table( lt(i), rt(i) ) );
         END LOOP;
         RETURN res; 
     end "nand";

     FUNCTION "or"  ( arg1,arg2 : UNSIGNED ) RETURN UNSIGNED IS
         CONSTANT ml  : integer := maximum( arg1'LENGTH, arg2'LENGTH );
         VARIABLE lt  : UNSIGNED ( 1 to ml );
         VARIABLE rt  : UNSIGNED ( 1 to ml );
         VARIABLE res : UNSIGNED ( 1 to ml );
     begin
         lt := zxt( arg1, ml );
         rt := zxt( arg2, ml );
         FOR i IN res'RANGE LOOP
             res(i) := or_table( lt(i), rt(i) );
         END LOOP;
         RETURN res; 
     end "or";

     FUNCTION "nor"  ( arg1,arg2 : UNSIGNED ) RETURN UNSIGNED IS
         CONSTANT ml  : integer := maximum( arg1'LENGTH, arg2'LENGTH );
         VARIABLE lt  : UNSIGNED ( 1 to ml );
         VARIABLE rt  : UNSIGNED ( 1 to ml );
         VARIABLE res : UNSIGNED ( 1 to ml );
     begin
         lt := zxt( arg1, ml );
         rt := zxt( arg2, ml );
         FOR i IN res'RANGE LOOP
             res(i) := not_table( or_table( lt(i), rt(i) ) );
         END LOOP;
         RETURN res; 
     end "nor";

     FUNCTION "xor"  ( arg1, arg2 : UNSIGNED ) RETURN UNSIGNED IS
         CONSTANT ml  : integer := maximum( arg1'LENGTH, arg2'LENGTH );
         VARIABLE lt  : UNSIGNED ( 1 to ml );
         VARIABLE rt  : UNSIGNED ( 1 to ml );
         VARIABLE res : UNSIGNED ( 1 to ml );
     begin
         lt := zxt( arg1, ml );
         rt := zxt( arg2, ml );
         FOR i IN res'RANGE LOOP
             res(i) := xor_table( lt(i), rt(i) );
         END LOOP;
         RETURN res; 
     end "xor";

     FUNCTION "not"  ( arg1 : UNSIGNED ) RETURN UNSIGNED IS
         VARIABLE result : UNSIGNED ( arg1'RANGE ) := (Others => 'X');
     begin
         for i in result'range loop
             result(i) := not_table( arg1(i) );
         end loop;
         return result;
     end "not";

     FUNCTION "and"  ( arg1,arg2 : SIGNED ) RETURN SIGNED IS
       CONSTANT len : INTEGER := maximum(arg1'length,arg2'length) ;
       VARIABLE a,b : UNSIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
       VARIABLE answer : SIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
     BEGIN
       a := (OTHERS => arg1(arg1'left)) ;
       a(arg1'length - 1 DOWNTO 0) := UNSIGNED(arg1);
       b := (OTHERS => arg2(arg2'left)) ;
       b(arg2'length - 1 DOWNTO 0) := UNSIGNED(arg2);
       answer := SIGNED(a and b);
       RETURN (answer);
     end "and";

     FUNCTION "nand"  ( arg1,arg2 : SIGNED ) RETURN SIGNED IS
       CONSTANT len : INTEGER := maximum(arg1'length,arg2'length) ;
       VARIABLE a,b : UNSIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
       VARIABLE answer : SIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
     BEGIN
       a := (OTHERS => arg1(arg1'left)) ;
       a(arg1'length - 1 DOWNTO 0) := UNSIGNED(arg1);
       b := (OTHERS => arg2(arg2'left)) ;
       b(arg2'length - 1 DOWNTO 0) := UNSIGNED(arg2);
       answer := SIGNED(a nand b);
       RETURN (answer);
     end "nand";

     FUNCTION "or"  ( arg1,arg2 : SIGNED ) RETURN SIGNED IS
       CONSTANT len : INTEGER := maximum(arg1'length,arg2'length) ;
       VARIABLE a,b : UNSIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
       VARIABLE answer : SIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
     BEGIN
       a := (OTHERS => arg1(arg1'left)) ;
       a(arg1'length - 1 DOWNTO 0) := UNSIGNED(arg1);
       b := (OTHERS => arg2(arg2'left)) ;
       b(arg2'length - 1 DOWNTO 0) := UNSIGNED(arg2);
       answer := SIGNED(a or b);
       RETURN (answer);
     end "or";

     FUNCTION "nor"  ( arg1,arg2 : SIGNED ) RETURN SIGNED IS
       CONSTANT len : INTEGER := maximum(arg1'length,arg2'length) ;
       VARIABLE a,b : UNSIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
       VARIABLE answer : SIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
     BEGIN
       a := (OTHERS => arg1(arg1'left)) ;
       a(arg1'length - 1 DOWNTO 0) := UNSIGNED(arg1);
       b := (OTHERS => arg2(arg2'left)) ;
       b(arg2'length - 1 DOWNTO 0) := UNSIGNED(arg2);
       answer := SIGNED(a nor b);
       RETURN (answer);
     end "nor";

     FUNCTION "xor"  ( arg1, arg2 : SIGNED ) RETURN SIGNED IS
       CONSTANT len : INTEGER := maximum(arg1'length,arg2'length) ;
       VARIABLE a,b : UNSIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
       VARIABLE answer : SIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
     BEGIN
       a := (OTHERS => arg1(arg1'left)) ;
       a(arg1'length - 1 DOWNTO 0) := UNSIGNED(arg1);
       b := (OTHERS => arg2(arg2'left)) ;
       b(arg2'length - 1 DOWNTO 0) := UNSIGNED(arg2);
       answer := SIGNED(a xor b);
       RETURN (answer);
     end "xor";

     FUNCTION "not"  ( arg1 : SIGNED ) RETURN SIGNED IS
         VARIABLE result : SIGNED ( arg1'RANGE ) := (Others => 'X');
     begin
         for i in result'range loop
             result(i) := not_table( arg1(i) );
         end loop;
         return result;
     end "not";

     FUNCTION "xnor"  ( arg1, arg2 : std_ulogic_vector ) RETURN std_ulogic_vector IS
         CONSTANT ml  : integer := maximum( arg1'LENGTH, arg2'LENGTH );
         VARIABLE lt  : std_ulogic_vector ( 1 to ml );
         VARIABLE rt  : std_ulogic_vector ( 1 to ml );
         VARIABLE res : std_ulogic_vector ( 1 to ml );
     begin
         lt := zxt( arg1, ml );
         rt := zxt( arg2, ml );
         FOR i IN res'RANGE LOOP
             res(i) := not_table( xor_table( lt(i), rt(i) ) );
         END LOOP;
         RETURN res; 
     end "xnor";

     FUNCTION "xnor"  ( arg1, arg2 : std_logic_vector ) RETURN std_logic_vector IS
         CONSTANT ml  : integer := maximum( arg1'LENGTH, arg2'LENGTH );
         VARIABLE lt  : std_logic_vector ( 1 to ml );
         VARIABLE rt  : std_logic_vector ( 1 to ml );
         VARIABLE res : std_logic_vector ( 1 to ml );
     begin
         lt := zxt( arg1, ml );
         rt := zxt( arg2, ml );
         FOR i IN res'RANGE LOOP
             res(i) := not_table( xor_table( lt(i), rt(i) ) );
         END LOOP;
         RETURN res; 
     end "xnor";

     FUNCTION "xnor"  ( arg1, arg2 : UNSIGNED ) RETURN UNSIGNED IS
         CONSTANT ml  : integer := maximum( arg1'LENGTH, arg2'LENGTH );
         VARIABLE lt  : UNSIGNED ( 1 to ml );
         VARIABLE rt  : UNSIGNED ( 1 to ml );
         VARIABLE res : UNSIGNED ( 1 to ml );
     begin
         lt := zxt( arg1, ml );
         rt := zxt( arg2, ml );
         FOR i IN res'RANGE LOOP
             res(i) := not_table( xor_table( lt(i), rt(i) ) );
         END LOOP;
         RETURN res; 
     end "xnor";

     FUNCTION "xnor"  ( arg1, arg2 : SIGNED ) RETURN SIGNED IS
       CONSTANT len : INTEGER := maximum(arg1'length,arg2'length) ;
       VARIABLE a,b : UNSIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
       VARIABLE answer : SIGNED(len-1 DOWNTO 0) := (OTHERS => '0') ;
     BEGIN
       a := (OTHERS => arg1(arg1'left)) ;
       a(arg1'length - 1 DOWNTO 0) := UNSIGNED(arg1);
       b := (OTHERS => arg2(arg2'left)) ;
       b(arg2'length - 1 DOWNTO 0) := UNSIGNED(arg2);
       answer := SIGNED(a xnor b);
       RETURN (answer);
     end "xnor";
     
END ;
