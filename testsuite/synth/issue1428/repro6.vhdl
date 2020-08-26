library ieee;
use ieee.std_logic_1164.all;

entity repro6 is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of repro6 is
  signal s : std_logic_vector(7 downto 0);
begin
  s (5 downto 0) <= b (5 downto 0);
  a <= '1' when s /= x"00" else '0';

  s (7 downto 4) <= "0000";
end behav;
