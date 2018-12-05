-------------------------------------------------------------------------------
--
--                                                       walter d. gallegos
--                                                   www.waltergallegos.com
--                                            Programmable Logic Consulting
--
-- Este archivo y documentacion son propiedad intelectual de Walter D. Gallegos
--
-------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL, IEEE.NUMERIC_STD.ALL;
USE STD.TextIO.ALL;

ENTITY LRAM IS
GENERIC ( size : INTEGER := 14; fName : STRING := "IntDemo.mem"; startAdr : INTEGER := 0);
   PORT (
      CLOCK : IN  STD_LOGIC;
         WR : IN  STD_LOGIC;
        BEA : IN  STD_LOGIC_VECTOR (3 DOWNTO 0);
        DIA : IN  STD_LOGIC_VECTOR (31 DOWNTO 0);
        DOA : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
       ADRA : IN  STD_LOGIC_VECTOR (size-1 DOWNTO 0);
        DOB : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
       ADRB : IN  STD_LOGIC_VECTOR (size-1 DOWNTO 0)
   );
END LRAM;

ARCHITECTURE WDG0 OF LRAM IS

   CONSTANT depth : INTEGER := (2**size)-1;
   TYPE LocalRAMDesc IS ARRAY (0 TO depth) OF STD_LOGIC_VECTOR(31 DOWNTO 0);

   IMPURE FUNCTION Init (fName : STRING) RETURN LocalRAMDesc IS
      FILE f : TEXT OPEN READ_MODE IS fName;
      VARIABLE l : LINE;
      VARIABLE b : STD_LOGIC_VECTOR(31 DOWNTO 0);
      VARIABLE m : LocalRAMDesc := (OTHERS => (OTHERS => '0'));
   BEGIN
      FOR i IN startAdr TO depth LOOP
         EXIT WHEN endfile(f);
         IF (i = depth) THEN
            REPORT "LRAM : Error memory full " SEVERITY FAILURE;
         END IF;
         readline(f, l); hread(l, b);
         m(i) := STD_LOGIC_VECTOR(RESIZE(UNSIGNED(b), 32));
      END LOOP;
      REPORT ("LRAM : loaded from " & fName & LF) SEVERITY NOTE;
      RETURN m;
   END FUNCTION;

   SIGNAL LocalRAM : LocalRAMDesc := Init(fName);
   SIGNAL data, code : STD_LOGIC_VECTOR(31 DOWNTO 0);

   -- XILINX ATTRIBUTE
   ATTRIBUTE ram_style : STRING;
   ATTRIBUTE ram_style OF LocalRAM : SIGNAL IS "block";
   -- XILINX END ATTRIBUTE

BEGIN

   CodeDataMemory : PROCESS(CLOCK)
   BEGIN
      IF rising_edge(CLOCK) THEN
         FOR i IN DIA'RANGE LOOP
            IF (BEA(i/8) AND WR)= '1' THEN
               LocalRAM(TO_INTEGER(UNSIGNED(ADRA)))(i) <= DIA(i);
            END IF;
         END LOOP;
         data <= LocalRAM(TO_INTEGER(UNSIGNED(ADRA)));
         code <= LocalRAM(TO_INTEGER(UNSIGNED(ADRB)));
      END IF;
   END PROCESS CodeDataMemory;

   DOA <= data;
   DOB <= code;

END WDG0;
