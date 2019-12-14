library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
  port (a, b : in std_logic_vector(7 downto 0);
        o, p : out std_logic);
end test;

architecture behav of test is
begin
  o <= or a;
  p <= and b;
end behav;
