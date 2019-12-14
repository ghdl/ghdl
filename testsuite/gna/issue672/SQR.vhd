-------------------------------------------------------------------------------
--                                                       walter d. gallegos    
--                                                   www.waltergallegos.com    
--                                             Programable Logic & Software    
--                                                     Consultoria y Diseno    
--                                                                             
-- Este archivo y documentacion son propiedad intelectual de Walter D. Gallegos
--                                                                             
-------------------------------------------------------------------------------
-- Autor : WDG 
-- Fecha : 2018-10-04
-- Archivo : SQR.vhd
-- Notas :
--	
-- 
-------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL, IEEE.NUMERIC_STD.ALL;


ENTITY SQR IS
   PORT(
      CLOCK : IN std_logic;
        DIN : IN std_logic_vector(31 downto 0);
        VIN : IN std_logic;
       DOUT : OUT std_logic_vector(31 downto 0);
       VOUT : OUT std_logic
   );
END ENTITY SQR;

ARCHITECTURE REV0 OF SQR IS

   CONSTANT busZero : STD_LOGIC_VECTOR(DIN'RANGE) := (OTHERS => '0');

   SIGNAL a : UNSIGNED(DIN'RANGE);
   SIGNAL c, sub : UNSIGNED(DIN'LEFT*2+1 DOWNTO 0);
   SIGNAL i : INTEGER RANGE a'RANGE;
   SIGNAL run : STD_LOGIC;

BEGIN

   sub <= c - a * a;

   Registers : PROCESS(CLOCK)
   BEGIN
      IF rising_edge(CLOCK) THEN
         IF VIN = '1' THEN 
            i <= a'LEFT; run <= '1'; 
            a <= (a'LEFT => '1', OTHERS => '0'); c <= UNSIGNED(DIN & busZero);   
         ELSIF run = '1' THEN
            IF i > 0 THEN i <= i - 1;            
               a(i) <= NOT(sub(sub'LEFT)); a(i-1) <= '1';             
            ELSE
               a(i) <= NOT(sub(sub'LEFT));               
               run <= '0';
            END IF;             
         END IF;
      END IF;
   END PROCESS Registers;

   DOUT <= STD_LOGIC_VECTOR(a); 
   VOUT <= NOT(run);

END REV0;

--ARCHITECTURE REV1 OF SQR IS
--
--   CONSTANT busZero : STD_LOGIC_VECTOR(17 DOWNTO 0) := (OTHERS => '0');
--   CONSTANT busZero2: STD_LOGIC_VECTOR(DIN'LEFT-18 DOWNTO 0) := (OTHERS => '0');
--
--   SIGNAL a : UNSIGNED(17 DOWNTO 0);
--   SIGNAL c, sub : UNSIGNED(a'LEFT*2+1 DOWNTO 0);
--   SIGNAL i : INTEGER RANGE a'RANGE;
--   SIGNAL run : STD_LOGIC;
--
--BEGIN
--
--   sub <= c - a * a;
--
--   Registers : PROCESS(CLOCK)
--   BEGIN
--      IF rising_edge(CLOCK) THEN
--         IF VIN = '1' THEN 
--            i <= a'LEFT; run <= '1'; 
--            a <= (a'LEFT => '1', OTHERS => '0'); c <= UNSIGNED(DIN(17 DOWNTO 0) & busZero);   
--         ELSIF run = '1' THEN
--            IF i > 0 THEN i <= i - 1;            
--               a(i) <= NOT(sub(sub'LEFT)); a(i-1) <= '1';             
--            ELSE
--               a(i) <= NOT(sub(sub'LEFT));               
--               run <= '0';
--            END IF;             
--         END IF;
--      END IF;
--   END PROCESS Registers;

--   DOUT <= busZero2 & STD_LOGIC_VECTOR(a); 
--   VOUT <= NOT(run);
--
--END REV1;