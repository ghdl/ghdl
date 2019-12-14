library ieee;
use ieee.std_logic_1164.all;

entity asgn02 is
  port (s0 : std_logic;
        r : out std_logic_vector (2 downto 0));
end asgn02;

architecture behav of asgn02 is
begin
  process (s0) is
  begin
    r <= "000";
    if s0 = '1' then
      r (1) <= '1';
    end if;
  end process;
end behav;
