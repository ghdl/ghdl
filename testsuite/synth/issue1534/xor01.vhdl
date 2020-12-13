library ieee;
use ieee.std_logic_1164.all;

entity xor01 is
    port (
        o: out bit_vector(3 downto 0)
    );
end entity;

architecture arch of xor01 is
begin
    o <= bit_vector'("1100") xor bit_vector'("1010");
end architecture;
