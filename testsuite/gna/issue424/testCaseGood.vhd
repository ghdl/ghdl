library ieee;
use ieee.std_logic_1164.all;

entity testCaseGood is
  port (outPad : out std_logic;
        inPad  : in  std_logic_vector(3 downto 0)
       );
end entity testCaseGood;

architecture behavioral of testCaseGood is
  component subBlock is
    port (outPort : out std_logic;
          inPort  : in  std_logic_vector(3 downto 0)
          );
  end component subBlock;
  
begin
  xsubBlock : subBlock
    port map (outPort => outPad,
              inPort  => inPad
             );
  
end architecture behavioral;
