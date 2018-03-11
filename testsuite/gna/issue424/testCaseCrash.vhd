library ieee;
use ieee.std_logic_1164.all;

entity testCaseCrash is
  port (outPad : out std_logic;
        inPad  : in  std_logic
       );
end entity testCaseCrash;

architecture behavioral of testCaseCrash is
  component subBlock is
    port (outPort : out std_logic;
          inPort  : in  std_logic
          );
  end component subBlock;
  
begin
  xsubBlock : subBlock
    port map (outPort => outPad,
              inPort  => inPad
             );
  
end architecture behavioral;
