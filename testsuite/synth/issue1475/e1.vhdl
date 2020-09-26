library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;

entity e1 is
end entity;

architecture a of e1 is
    signal s : unsigned(31 downto 0) := (others => '0');
begin
    assert s = -3;
end architecture;
