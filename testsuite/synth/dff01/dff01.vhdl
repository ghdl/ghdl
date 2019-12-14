library ieee;
use ieee.std_logic_1164.all;

entity dff01 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end dff01;

architecture behav of dff01 is
begin
  process (clk) is
  begin
    if rising_edge (clk) then
      q <= d;
    end if;
  end process;
end behav;
