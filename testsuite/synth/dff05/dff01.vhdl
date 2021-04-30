library ieee;
use ieee.std_logic_1164.all;

entity dff01 is
  port (q : out std_logic;
        d : std_logic;
        clk : std_logic);
end dff01;

architecture behav of dff01 is
begin
  q <= d when rising_edge (clk);
end behav;
