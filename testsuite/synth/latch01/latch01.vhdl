library ieee;
use ieee.std_logic_1164.all;

entity latch01 is
  port (q : out std_logic;
        d : std_logic;
        en : std_logic);
end latch01;

architecture behav of latch01 is
begin
  process (en, d) is
  begin
    if en = '1' then
      q <= d;
    end if;
  end process;
end behav;
