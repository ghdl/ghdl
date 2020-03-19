library ieee;
use ieee.std_logic_1164.all;

entity issue1 is
   port (foo : out std_logic_vector(4-1 downto 0));
end issue1;

architecture rtl of issue1 is
begin
    foo <= ("0",others=>'1');
end architecture;
