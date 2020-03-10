library ieee;
use ieee.std_logic_1164.all;

entity if03 is
  port (a : std_logic;
        b : std_logic;
        sel : std_logic;
        s : out std_logic);
end if03;

architecture behav of if03 is
begin
  process (a, b, sel)
  begin
    if sel = '0' then
      s <= a;
    elsif sel = '1' then
      s <= b;
    end if;
  end process;
end behav;
