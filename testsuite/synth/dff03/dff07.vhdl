library ieee;
use ieee.std_logic_1164.all;

entity dff07 is
  port (q1, q2 : out std_logic;
        d : std_logic;
        en1 : std_logic;
        en2 : std_logic;
        clk : std_logic);
end dff07;

architecture behav of dff07 is
begin
  process (clk) is
  begin
    if rising_edge (clk) and en1 = '1' then
      if en2 = '1' then
        q1 <= d;
        q2 <= d;
      end if;
    end if;
  end process;
end behav;
