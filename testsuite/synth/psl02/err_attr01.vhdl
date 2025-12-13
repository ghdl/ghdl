library IEEE;
use IEEE.std_logic_1164.ALL;

entity err_attr01 is
  port (clk : std_logic);
end;

architecture behav of err_attr01 is
  signal any_nat : natural;

  attribute anyconst : bit;
  attribute anyconst of any_nat : signal is '1';
begin
  default Clock is rising_edge(clk);

  assert always any_nat >=10;
end architecture behav;

