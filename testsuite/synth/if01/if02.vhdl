library ieee;
use ieee.std_logic_1164.all;

entity if02 is
  port (c : std_logic_vector(7 downto 0);
        s : std_logic;
        r : out std_logic_vector(7 downto 0));
end if02;

architecture behav of if02 is
begin
  process (c, s)
  begin
    if s = '0' then
      r (6 downto 0) <= c (7 downto 1);
      r (7) <= c (0);
    else
      r (0) <= c (7);
      r (7 downto 1) <= c (6 downto 0);
    end if;
  end process;
end behav;
