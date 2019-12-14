library ieee;
use ieee.std_logic_1164.all;

entity repr is
end entity;

architecture arch of repr is

    constant CYCLE 	: time := 10 ns;

    signal vec1 :  std_logic_vector(31 downto 0);
    signal vec2 :  std_logic_vector(31 to 0);
begin
    vec1 <= (others => '0');
    vec2 <= (others => '0');

end arch;
