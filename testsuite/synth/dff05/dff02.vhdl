library ieee;
use ieee.std_logic_1164.all;

entity dff02 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic;
        rst : std_logic);
end dff02;

architecture behav of dff02 is
begin
  q <= '0' when rst = '1' else d when rising_edge (clk);
end behav;
