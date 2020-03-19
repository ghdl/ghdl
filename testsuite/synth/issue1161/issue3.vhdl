library ieee;
use ieee.std_logic_1164.all;

entity issue3 is
   port (foo : out std_logic_vector(4-1 downto 0));
end issue3;

architecture rtl of issue3 is
begin
    foo <= ("01", "10");
end architecture;
