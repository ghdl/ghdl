library ieee;
use ieee.std_logic_1164.all;

entity asgn01 is
  port (a : std_logic_vector (2 downto 0);
        s0 : std_logic;
        r : out std_logic_vector (2 downto 0));
end asgn01;

architecture behav of asgn01 is
begin
  process (a, s0) is
  begin
    if s0 = '1' then
      r <= "000";
    else
      r <= a;
    end if;
  end process;
end behav;
