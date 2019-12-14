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
-- Fecha : 2018-09-21
-- Archivo : SPITb.vhd
-- Notas :
--	
-- 
-------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL, IEEE.NUMERIC_STD.ALL;

ENTITY SQRTb IS
END SQRTb;

ARCHITECTURE TB OF SQRTb IS
   
   CONSTANT tCyc : TIME := 50 ns;

   COMPONENT SQR
   PORT (    
      CLOCK : IN std_logic;
        DIN : IN std_logic_vector(31 downto 0);
        VIN : IN std_logic;
       DOUT : OUT std_logic_vector(31 downto 0);
       VOUT : OUT std_logic
      );
   END COMPONENT SQR;
    
   SIGNAL clock, vIn, vOut : STD_LOGIC;
   SIGNAL dIn, dOut : STD_LOGIC_VECTOR (31 DOWNTO 0);   

   PROCEDURE Transfer( 
      d : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
      SIGNAL clock : IN STD_LOGIC;
      SIGNAL di : OUT STD_LOGIC_VECTOR(31 DOWNTO 0);
      SIGNAL vOut : IN STD_LOGIC;      
      SIGNAL vIn : OUT STD_LOGIC) IS
   BEGIN
      WAIT UNTIL rising_edge(CLOCK);
      WAIT FOR 1 ns; di <= d; 
      WAIT FOR 1 ns; Vin <= '1'; 
      WAIT FOR tCyc; vIn <= '0';
      WAIT FOR 1 ns;
      WAIT UNTIL vOut = '1';
   END PROCEDURE Transfer;
   
BEGIN

   ClockGen : PROCESS
   BEGIN
      clock <= '0'; WAIT FOR tCyc/2;
      clock <= '1'; WAIT FOR tCyc/2;
   END PROCESS ClockGen;
            
   A0 : SQR
   PORT MAP (    
      CLOCK => clock,
        DIN => dIn,
        VIN => vIn,
       DOUT => dOut,
       VOUT => vOut
      );
      
   Stim : PROCESS
   BEGIN
      vIn <= '0'; 
      WAIT FOR 100 ns; Transfer(x"00002000", clock, dIn, vOut, vIn );
      WAIT FOR 200 ns; Transfer(x"00003000", clock, dIn, vOut, vIn );
      WAIT FOR 100 ns; Transfer(x"00004000", clock, dIn, vOut, vIn );
      WAIT FOR 100 ns; Transfer(x"08004000", clock, dIn, vOut, vIn );
      WAIT;
   END PROCESS Stim;

END TB;

CONFIGURATION Default OF SQRTb IS
   FOR TB
      FOR A0 : SQR
         USE ENTITY work.SQR(REV0); 
      END FOR;
   END FOR;
END Default;