library ieee;
use ieee.std_logic_1164.all;

entity output06 is
  port (i : std_logic;
        o : out std_logic_vector (3 downto 0));
end output06;

architecture behav of output06 is
  signal s : std_logic_vector(3 downto 0);
begin
  process (i)
  begin
    s(0) <= i;
    s (1) <= not i;
    s (3) <= i;
  end process;

  s (2) <= '0';

  o <= s;
end behav;

