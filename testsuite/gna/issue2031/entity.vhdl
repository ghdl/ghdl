library IEEE;
use IEEE.std_logic_1164.all;

entity abc is
    port(
        a : std_logic;
        b : std_logic;
        c : out std_logic
    );
end entity;

architecture arch of abc is
begin
    c <= a and b;
end architecture;
