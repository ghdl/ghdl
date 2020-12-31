library ieee;
use ieee.std_logic_1164.all;

entity assume0 is
  port (
    clk : in  std_logic;
    i   : out integer
  );
end assume0;

architecture behav of assume0 is

begin

  i <= 1;

  default clock is rising_edge(clk);

  psl_a : assume always i = 1;

end behav;
