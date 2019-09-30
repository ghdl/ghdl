library ieee;
use ieee.std_logic_1164.all,
    ieee.numeric_std.all;

entity ent is
end;

architecture a of ent is
    signal x : unsigned(7 downto 0);
    signal y : unsigned(7 downto 0) := x / 2;
    signal z : unsigned(15 downto 0) := x * 2;
begin
end;
