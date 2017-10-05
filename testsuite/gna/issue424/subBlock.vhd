library ieee;
use ieee.std_logic_1164.all;

entity subBlock is
    port (outPort : out std_logic;
          inPort  : in  std_logic_vector(3 downto 0)
          );
end entity subBlock;

architecture behavioral of subBlock is

begin
    outPort <= inPort(0);
  
end architecture behavioral;
