library ieee;
use ieee.std_logic_1164.all;

entity repro3 is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of repro3 is
  signal s : std_logic_vector(7 downto 0);
begin
  s <= b;
  a <= '1' when s /= x"00" else '0';

  s (5) <= '0';
end behav;
