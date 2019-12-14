library ieee;
use ieee.std_logic_1164.all;

entity asgn05 is
  port (s0 : std_logic;
        s1 : std_logic;
        r : out std_logic_vector (5 downto 0));
end asgn05;

architecture behav of asgn05 is
begin
  process (s0, s1) is
  begin
    r <= "000000";
    if s0 = '1' then
      r (1) <= '1';
      r (3) <= '1';
      r (4 downto 2) <= "101";
    end if;
  end process;
end behav;
