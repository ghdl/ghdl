library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity if01 is
  port (c0, c1 : std_logic;
        r : out std_logic);
end if01;

architecture behav of if01 is
begin
  process (c0, c1)
  begin
    r <= '0';
    if c0 = '1' then
      if c1 = '1' then
        r <= '1';
      end if;
    end if;
  end process;
end behav;
