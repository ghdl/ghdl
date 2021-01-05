library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity assert01 is
  port (
    a, b : out std_logic);
end;

architecture behav of assert01 is
  signal s : std_logic;
begin
  assert ((a and b and s) = '0') report "error" severity error;
end;
