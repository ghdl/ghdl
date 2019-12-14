library ieee;
use ieee.std_logic_1164.all;

entity dff05 is
  port (q : out std_logic;
        d : std_logic;
        en1 : std_logic;
        en2 : std_logic;
        clk : std_logic);
end dff05;

architecture behav of dff05 is
begin
  process (clk) is
  begin
    if en2 = '1' and (en1 = '1' and rising_edge (clk)) then
      q <= d;
    end if;
  end process;
end behav;
