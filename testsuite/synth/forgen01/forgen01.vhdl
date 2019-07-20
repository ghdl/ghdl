library ieee;
use ieee.std_logic_1164.all;

entity forgen01 is
  port (a : out std_logic_vector (7 downto 0));
end;

architecture behav of forgen01 is
  constant c : std_logic_vector (7 downto 0) := x"a1";
begin
  gen: for i in a'range generate
    a (i) <= c (i);
  end generate;
end behav;
