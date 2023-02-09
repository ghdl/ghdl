LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.STD_LOGIC_ARITH.ALL;
USE IEEE.STD_LOGIC_UNSIGNED.ALL;

LIBRARY work;

ENTITY ent IS

  PORT (not_reset      : IN  STD_LOGIC;
        clock          : IN  STD_LOGIC;
        inp  : IN  integer;
        outp : OUT STD_LOGIC_VECTOR(31 downto 0));
END ent ;

ARCHITECTURE rtl OF ent IS


BEGIN

         PROCESS (not_reset,clock)
         BEGIN
         
             IF(not_reset='0') THEN
                 
               outp <= CONV_STD_LOGIC_VECTOR(CONV_UNSIGNED(0,32),32);
--               outp <= STD_LOGIC_VECTOR(CONV_UNSIGNED(0,32));
      
             ELSIF(clock'EVENT AND clock='1') THEN
      
               outp <= CONV_STD_LOGIC_VECTOR(CONV_UNSIGNED(inp,32),32);
--               outp <= STD_LOGIC_VECTOR(CONV_UNSIGNED(inp,32));
      
             END IF;
      
         END PROCESS;
      
      
END rtl;


