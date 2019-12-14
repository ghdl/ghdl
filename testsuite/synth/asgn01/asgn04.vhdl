library ieee;
use ieee.std_logic_1164.all;

entity asgn04 is
  port (s0 : std_logic;
        s1 : std_logic;
        r : out std_logic_vector (2 downto 0));
end asgn04;

architecture behav of asgn04 is
begin
  process (s0, s1) is
  begin
    r <= "000";
    if s0 = '1' then
      r (1) <= '1';
      if s1 = '1' then
        r(1 downto 0) <= "01";
      end if;
    end if;
  end process;
end behav;
