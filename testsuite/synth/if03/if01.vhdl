library ieee;
use ieee.std_logic_1164.all;

entity if01 is
  port (a : std_logic;
        b : std_logic;
        en1 : std_logic;
        sel1 : std_logic;
        clk : std_logic;
        s1 : out std_logic;
        s2 : out std_logic);
end if01;

architecture behav of if01 is
begin
  process (clk) is
    variable t : std_logic;
  begin
    if rising_edge(clk) then
      if en1 = '1' then
        t := b;
        s1 <= a;
      end if;
      s2 <= t;
    end if;
  end process;
end behav;
