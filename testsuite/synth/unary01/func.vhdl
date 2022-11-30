library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity func is
  port (a, b : in std_logic_vector(7 downto 0);
        o, p : out std_logic);
end func;

architecture behav of func is
begin
  o <= "or"(a);
  p <= "and"(b);
end behav;
