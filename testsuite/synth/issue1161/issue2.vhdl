library ieee;
use ieee.std_logic_1164.all;

entity issue2 is
   port (foo : out std_logic_vector(4-1 downto 0));
end issue2;

architecture rtl of issue2 is
begin
    foo <= (2 downto 1 => "00" ,others=>'1');
end architecture;
