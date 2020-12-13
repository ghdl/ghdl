library ieee;
use ieee.std_logic_1164.all;

entity xor02 is
    port (
        o: out bit
    );
end entity;

architecture arch of xor02 is
    constant x: bit_vector(0 to 0) := "1";
    constant y: bit_vector(0 to 0) := "0";
begin
    o <= x(0) xor y(0);
end architecture;
