library ieee;
use ieee.numeric_std.all;

entity e is
end entity;

architecture a of e is
    signal s : unsigned(31 downto 0);
begin
    assert s = -3;
end architecture;
