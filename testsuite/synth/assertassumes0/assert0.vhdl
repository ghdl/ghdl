library ieee;
use ieee.std_logic_1164.all;

entity assert0 is
  port (
    clk : in  std_logic;
    i   : out integer
  );
end assert0;

architecture behav of assert0 is

begin

  i <= 1;

  default clock is rising_edge(clk);

  psl_a : assert always i = 1;

end behav;
