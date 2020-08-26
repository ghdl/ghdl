library ieee;
use ieee.std_logic_1164.all;

entity repro5b is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of repro5b is
  signal s : std_logic_vector(1 to 8);
begin
  s <= b;
  a <= '1' when s /= x"00" else '0';

  s (2 to 4) <= "000";
end behav;
